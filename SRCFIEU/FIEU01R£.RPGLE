000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600060609      *  TRASCODIFICA : IGATEAVIS                                               *
000700060609      *               BOLLE IMPORT DPD nuovo parcel e depot                     *
000800060612      **?************************************************************************
000900991124      * Il pgm crea:                                                            *
001000020919      *             EDivab3L file spedizioni da confermare per camion           *
001100020919      *             FiVAB01L file spedizioni da confermare                      *
001200060609      **?************************************************************************
001300060612     Ftivin00r  uF   E             DISK    usropn
001400991124      *
001500991206     FFLNUF01L  UF   E           K DISK
001600130530     FFnorm01L  iF   E           K DISK
001700991129     FTNTBE01L  IF   E           K DISK
001800050112     FTABEL00F  IF   E           K DISK
001900130618     FazORG01L  IF   E           K DISK
002000971216      *
002100150604     FTNCLD00F  IF   E           K DISK
002200061016     Ffidpo06l  IF   E           K DISK
002300141103     Ffnblp01l  IF   E           K DISK
002400110930     Ffiar403l  IF   E           K DISK
002500110930     FEDivat4L  iF   E           K DISK    rename(EDIVAT00:EDIVAT4)
002600110930     FFiVAT02L  if   E           K DISK    rename(FIVAT000:FIVAT02)
002700061016      *
002800050414     FFiVAB01L  UF A E           K DISK
002900050414     FEDivab3L  UF A E           K DISK
003000160120     FEDivab2L  UF   E           K DISK    rename(EDIVAB00:EDIVAB2)
003100041215      *
003200050414     FEDiVAT3L  Uf A E           K DISK
003300050414     FFiVAT01L  Uf A E           K DISK
003400940321      *----------------------------------------------------*
003500991124      * Schiere per conversione peso DPD
003600991124     D DPPK            S             15    DIM(30)                              CODICE NAZIONE
003700991124     D DPP             S              7S 1 DIM(30)                              CODICE NAZIONE
003800141210     D VLB             S              5S 3 DIM(30)                              CODICE NAZIONE
003900971210      *-------------------
004000000907      * schiere dei Depositi DPD dai quali rivavare la linea
004100051219     D DEPK            S             15    DIM(300)                             COD DEP
004200091106     D DEPK2           S             30    DIM(300)                             COD DEP + HUB
004300051219     D DepL            S              3S 0 DIM(300)                             COD Linea
004400051219     D DepC            S              7S 0 DIM(300)                             COD Cliente
004500051219     D DepT            S              3s 0 DIM(300)                             COD tariffa
004600150604     D DepTvtg         S              3s 0 DIM(300)                             COD tariffa VTG
004700051219     D DepM            S              2A   DIM(300)                             COD trat.merce
004800051219     D DepW            S              1A   DIM(300)                             EDIVAB --> 'S'
004900000907      *-------------------
005000060612     D Lung_Parcel     S              3  0
005100060614     D Tipo_error      S              1  0
005200060612      *-------------------
005300991129     D W015A           S             15
005400091109     D W030A           S             30
005500000223     D W0140           S             14  0
005600991129     D WORA            S              6  0
005700991129     D XX              S              3  0 INZ
005800991129     D WDTGIO          S              8  0
005900991129     D DATEU           S              8  0
006000141103     D DATEUMENOGG     S              8  0
006100991129     D DATA_eur        S               D   DATFMT(*eur)
006200141103     D DATA_limit_BLP  S               D   DATFMT(*iso)
006300991206     D KAAA            S                   LIKE(NUFAAA)
006400991206     D KCNU            S                   LIKE(NUFCNU)
006500991206     D KFIL            S                   LIKE(NUFFIL)
006600050112     D NUM_Sped        s                   LIKE(vabnsp)
006700160120     d  Sosp_AAS       s                   like(VATAAS)
006800160120     d  Sosp_LNP       s                   like(VATLNP)
006900160120     d  Sosp_NRS       s                   like(VATNRS)
007000160120     d  Sosp_NSP       s                   like(VATNSP)
007100050112     D svkpjbu         s                   like(kpjbu)
007200060413      *-------------------
007300991129     D KPJBA         E DS
007400000406     D DDPP          E DS
007500000907     D DDEP          E DS
007600130618     D OG143         E DS
007700141103     D DS3IDP        E DS
007800160301     D Dlpd          E DS
007900060608      **?------------------------------------------------------------------ */
008000060608     C*? Ds Scrittura del VAT "E"  passaggio nuovi campi BIC3
008100060608      **?------------------------------------------------------------------ */
008200060612     D XTOEBCDDS     E DS
008300060612     D DVate         E DS
008400060612     D TIDP100       E DS                  EXTNAME(TIDP100W)
008500060620      *
008600060620     D DoveVado        DS
008700060620     d  Italia39                      2
008800060620     d  CapItalia                     5
008900060620     d  TipoPacco                     2
009000060620     d  ChkDigit                      1
009100060620      *
009200060620     D DsParcel        DS
009300060620     d  Parcel0                       1
009400060620     d  Depot3                        3
009500060620     d  NoveNove                      2
009600060620     d  NumParcel                     8
009700060620      *
009800060620     D Parcel11        S             11
009900060710     D Parcel14        S             14
010000150604     D Depot04         S              4
010100150604      *
010200060911     D DepotParcel     S              3
010300060620      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
010400060620     D                 DS
010500060620     D  DPDLNP                 1      3  0
010600060620     D  DPDGMS                 4      7  0
010700060620     D  DPDNDS                 8     11  0
010800060620     D  §11                    1     11  0
010900060620     D                                     DIM(11)
011000060620     D  DPDCHD                12     12  0
011100060620     D  DPDBRC                 1     12  0
011200060608      **?------------------------------------------------------------------ */
011300050112      * Numeratore Bolle (302)
011400050112     D trul33ds      E DS
011500050112     D Ds3C          E DS
011600070202     D TISI95DS      E DS
011700060612      * ?================================================================== */
011800060612      * ?   * Campi da tradurre con interi
011900060612     D atoi            PR            10I 0
012000060612     D                                     EXTPROC('atoi')
012100060612     D  Char                           *
012200060612     D                                     VALUE
012300060612     D                                     OPTIONS(*STRING)
012400060612      * ?   * Campi da tradurre con decimali
012500060612     D atof            PR             8F
012600060612     D                                     EXTPROC('atof')
012700060612     D  Char                           *
012800060612     D                                     VALUE
012900060612     D                                     OPTIONS(*STRING)
013000060612      * ?   *--------------------------------------------------------------*
013100060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
013200060612      * ?   *--------------------------------------------------------------*
013300060612      *
013400060612      * ?   * Campi x decodifica * (INPUT  del Record)
013500060612     D  Dati           s           1500
013600060612     D Separa_campi    s              1
013700060612     D Decimal_Separ   s              1
013800060612      * ?   * Campi decodificati * (OUTPUT del Record)
013900060612      *       Schiere di Output / Campi di Output
014000060614     D  Alfa40         s             40
014100060612     d  Sk             s              3u 0
014200060612     D    NR_campi     s              3u 0
014300060612     D  Campi_Record   ds
014400060612     D    Dato_sk                    40    DIM(100)
014500060612     D  Campi_Lunghi   ds
014600060612     D    LunDato_sk                  3u 0 DIM(100)
014700060612     D Campi_Numerici  ds
014800060612     D    DatoNum_sk                  1    DIM(100)
014900060612     D Campi_Decimali  ds
015000060612     D    Decimal_sk                  3u 0 DIM(100)
015100060612      * ?   *--------------------------------------------------------------*
015200060612     D  position       s              3  0 INZ(0)
015300060612
015400060612     D  se_errore      s              1    inz(' ')
015500060612     D  Msg_Err        s            132    inz(' ')
015600060614     D  Msg_Vin_80     s             80    inz(' ')
015700060612      * ?* ------------------------------------------------------ *
015800060612      * Records di controllo mandati nell'IGATEAVIS
015900060612      * ?   * Dati Record UNO *
016000060612     DDepot_Trs_UNO    s             10
016100060612     D User_Trs_UNO    s             10
016200060612     D Data_Trs_UNO    s              8  0
016300060612     D Time_Trs_UNO    s              6  0
016400060612     D  Num_Trs_UNO    s              9  0
016500060612      * ?   * Dati Record DUE *
016600060612     D Testa_Sk        s             10    DIM(50)
016700060612      * ?   * Dati Record TRE *
016800060612     D  Num_Trs_FIN    s              9  0
016900120504     d non_inviato_prima...
017000120504     d                 s              1
017100060710      **
017200140827     d non_completamente_tradotto...
017300140827     d                 s              1    inz(' ')
017400140828     d Identifica_file...
017500140828     d                 s            100    inz(' ')
017600140828     d Identifica_record...
017700140828     d                 s            250    inz(' ')
017800140827      **
017900060710     D Digits          C                   '0123456789'
018000130819     d Normal_Parcel   c                   101
018100130819     d Small_Parcel    c                   136
018200130819     d Return_Sender   c                   298
018300130819     d Return          c                   300
018400140409     d Gomme_365       c                   365
018500140409     d Gomme_366       c                   366
018600140409     d Gomme_367       c                   367
018700111110     d Normal_Parcel_327...
018800130819     d                 c                   327
018900111110     d Normal_Parcel_329...
019000130819     d                 c                   329
019100111110     d Small_Parcel_328...
019200130819     d                 c                   328
019300160120     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
019400111006      *
019500060612      * ?================================================================== */
019600060612      *   Ciclo principale
019700060413      * ?********************************************************************/
019800060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
019900060612     C*
020000060612     C                   if        not %open(tivin00r)
020100060612     C                   open      tivin00r
020200060612     C                   endif
020300060612      **
020400060612      * ?------------------------------------------------------------------ */
020500060612     C*? Controllo dati arrivati da DPD
020600060612      * ?------------------------------------------------------------------ */
020700060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
020800060612      * ?- controllando che la trasmissione sia completa.
020900060612
021000060612      * ?              /*---------------------- */
021100060612     c                   exsr      check_Trasm
021200060612      * ?              /*---------------------- */
021300060612
021400060612      **
021500060613      **  Errore di trasmissione x tutti i records
021600060613      **   --> file in errore
021700060613     c                   if        se_errore = 'S'
021800060612
021900060612
022000060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
022100060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
022200060613     C                               >> Farsi rimandare i DATI !!'
022300060612
022400060612      * ?  Scrive su tutti i records il tipo di errore
022500060612     c     *start        setll     tivin00r
022600060612     c                   read      tivin00r
022700060612     c                   dow       not %eof(tivin00r)
022800060612     C                   eval      vinMSG = Msg_Err
022900060612     C                   eval      vinFLG = '2'
023000060613     c                   eval      esito  = '2'
023100060612     c                   update    tivin000
023200060612     c                   read      tivin00r
023300090605     c                   endDO
023400090605      **
023500090605      **    avviso tramite MAIL
023600090605     C                   exSR      eMail_alert
023700060612      **
023800060612     c                   else
023900060614      **
024000060612      * ?------------------------------------------------------------------ */
024100060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
024200060612      * ?------------------------------------------------------------------ */
024300060612      * ?              /*---------------------- */
024400060612     c                   exsr      Importa_Msg
024500060612      * ?              /*---------------------- */
024600060612
024700060614     c                   end
024800060614
024900060612     C                   if        %open(tivin00r)
025000060612     C                   close     tivin00r
025100060612     C                   endif
025200060612      *
025300060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
025400060614      *
025500110930     c*********          if        almeno_uno ='S' and esito ='1'
025600110930     C*********          eval      esito ='0'
025700110930     C*********          endif
025800060614      *
025900140827     C                   if        non_completamente_tradotto = 'S'
026000140827     c                   exsr      email_alert
026100140827     c                   end
026200140827      *
026300060612     c                   SETON                                        LR
026400060612      * ?================================================================== */
026500060612      *? Controlla la trasmissione   se completa
026600060612      * ?------------------------------------------------------------------ */
026700060612     c     Check_Trasm   Begsr
026800060612
026900060612     C                   clear                   se_errore
027000080917     C                   clear                   Riga_iNIZIo       1
027100080917     C                   clear                   Riga_fINe         1
027200060612      ** primo  record
027300060612     c     *start        setll     tivin00r
027400071025     c     rileggi       tag
027500060612     c                   read      tivin00r
027600060612     c                   if        not %eof(tivin00r)
027700071025      *
027800071025     c                   movel(p)  VINDTA        dati
027900071025     c                   if        dati = *blank
028000071025      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
028100071025      * le leggo e le escludo.
028200071025     C                   eval      vinFLG = '1'
028300071025     c                   update    Tivin000
028400071025     c                   goto      rileggi
028500071025     c                   end
028600060612      * ?              /*---------------------- */
028700060612     c                   exsr      Decod_Record
028800060612      * ?              /*---------------------- */
028900060612     c                   endif
029000060612      **
029100060612      ** ultimo record
029200060612     c     *hival        setll     tivin00r
029300080917     c     leggiAncora   tag
029400060612     c                   readp     tivin00r
029500060612     c                   if        not %eof(tivin00r)
029600080917      *
029700080917     c                   movel(p)  VINDTA        dati
029800080917     c                   if        dati = *blank
029900080917      * le ultime righe potrebbero essere vuote prima di finire il messaggio
030000080917      * le leggo e le escludo.
030100080917     C                   eval      vinFLG = '1'
030200080917     c                   update    Tivin000
030300080917     c                   goto      leggiAncora
030400080917     c                   end
030500060612      * ?              /*---------------------- */
030600060612     c                   exsr      Decod_Record
030700060612      * ?              /*---------------------- */
030800060612     c                   endif
030900060612
031000060612      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
031100060612      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
031200060612      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
031300080917     C******             if        num_trs_UNO <> num_trs_FIN
031400080917     C****** Non è + vero poichè DPD invia trasmissioni di + trasmissioni quindi
031500080917     C****** Non si può confrontare il numero trsm.
031600080917     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
031700060612      * ?-----> Errore
031800060612     C                   eval      se_errore = 'S'
031900060612
032000060612     c                   end
032100060612
032200060612     c                   endSR
032300060612      * ?------------------------------------------------------------------ */
032400060612      *? Importa i records della tramsissione
032500060612      * ?------------------------------------------------------------------ */
032600060612     c     Importa_Msg   Begsr
032700060612
032800060614      * per controllare se almeno un record è stato importato sul VAB
032900060614     c                   clear                   Almeno_Uno        1
033000060614
033100060612     c     *start        setll     Tivin00r
033200060612     c                   read      Tivin00r
033300060612
033400060612     c                   dow       not %eof(Tivin00r)
033500060614
033600060614      * solo i record sflaggati da rielaborare
033700060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
033800060612
033900060612      ** Controlli formali sui campi
034000060612     c                   clear                   se_errore
034100060620     C                   clear                   Msg_Vin_80
034200060612
034300060612      ** Decodifica record a campi variabili
034400060612      * ?              /*---------------------- */
034500060612     c                   exsr      Decod_Record
034600060612      * ?              /*---------------------- */
034700060612
034800060612      **  Se presente un errore nel record emette una segnalazione msg
034900060612     c                   if        se_errore ='S'
035000060614     C                   eval      vinMSG = Msg_Vin_80
035100060621     c                   end
035200140827      *
035300140827      * Dopo il controllo Formale di RIGA occorre quindi filtrare quando
035400140827      *  DEVE saltare alla riga successiva ricevuta e NON FARE nulla.
035500140827      *  ma deve dare solo l'errore.
035600140827     c                   If        salta_record = *blank
035700110929      *
035800110929      * ?              /*---------------------- */
035900110929      *  Controlla se il Parcel non fosse già presente su EDIVAT o FIVAT
036000110929      * ?              /*---------------------- */
036100110930     c                   clear                   destino          35
036200110929      *
036300110930      *  SE GIA' PRESENTE NON VIENE CARICATO
036400110930     c                   if        %subst(dati:1:1) <> '#'
036500110930      *
036600110930      *  Controlla se la spedizione è già arrivata precedentemente
036700110930      *    sulle bolle in partenza o da confermare
036800160120      *  Oppure se è in attesa poichè arrivata da GEODATA senza ancora lo SCAN 05
036900160120      *  ossia in ATTESA dello SCAN 05.
037000110930     c                   clear                   GIA_PRESENTE      1
037100160120     c                   clear                   IN_ATTESA         1
037200111006      *
037300111006      **  se non è un reso  controlla se già arrivato
037400111006     c                   if        I1SERVICE <>  300
037500110930     c                   EXSR      check_PARCEL
037600111006     c                   end
037700160120      * -----
037800110930      *  se fosse già presente
037900110930     c                   IF           GIA_PRESENTE = 'S'
038000110929     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(i1PARCEL) +
038100110929     c                                          ' già ricevuto precedentemente!'
038200110929     C                   eval      vinMSG = Msg_Vin_80
038300160120      * -----
038400160120     c                   elseIF       IN_ATTESA = 'S'
038500160120      * ?              /*---------------------- */
038600160120     c                   exsr      UPD_DA_GEO
038700160120      * ?              /*---------------------- */
038800160120     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(i1PARCEL) +
038900160120     c                                          ' in Attesa da GEO! Ora in elab-
039000160208     c                             orazione'
039100160120     C                   eval      vinMSG = Msg_Vin_80
039200160120      * -----
039300110929     c                   else
039400160120      * -----
039500060612      *  con il record in canna scrive direttamente il VAB ed il VAT
039600060612      * ?              /*---------------------- */
039700060612     c                   exsr      WRT_VAB_VAT
039800060612      * ?              /*---------------------- */
039900060614      *  Problemi nella decodifica dei campi VAB/VAT
040000060614     c                   if        se_errore ='S'
040100060614     C                   eval      vinMSG = Msg_Vin_80
040200060614     c                   end
040300111110
040400110929     c                   endIF
040500160120
040600110930     c                   endIF
040700060612
040800060621      *  Sempre Record OK
040900140827     c                   endIF
041000140827
041100060621     C                   eval      vinFLG = '1'
041200060621
041300060621      *  x errore bloccante nella composizione del VAB/VAT
041400060621     c                   if        err_bloccante ='S'
041500060621     C                   eval      vinFLG = '2'
041600060710     c                   eval      esito  = '1'
041700060621     c                   end
041800060612
041900060612     c                   update    Tivin000
042000060614     c                   endIF
042100060612
042200060612     c                   read      Tivin00r
042300060612     C                   ENDdo
042400060612      **
042500060612     c                   endSR
042600110930      * ?------------------------------------------------------------------ */
042700110930      *?    CONTROLLA SE il parcel è arrivato in una precedente trasmissione
042800110930      * ?------------------------------------------------------------------ */
042900110930     c     check_PARCEL  Begsr
043000130619      **
043100130619      ** controlla la Linea che deve essere DPD
043200130619     c                   z-add     0             LineaP            3 0
043300110930      **
043400110930      **  se non ancora confermato CMR
043500110930     C                   eval        vatTRC = 'E'
043600110930     C                   eval        vatNOT = i1PARCEL
043700110930     C     Kvat4E        setll     edivat4l
043800110930     C     vatTRC        reade     edivat4l
043900110930     c                   IF        not %EoF(edivat4l) AND
044000110930     C                             I1PARCEL = %SUBST(vatNOT:1:14)
044100160120      *--
044200160120      *-   Se era in Sospeso perchè scritto dalla parte del GEODATA
044300160120      *-   in attesa dello SCAN 05 di (Partito dal DEPOT di PARTENZA quindi in viaggio)
044400160120      *--
044500160120     c                   if        vatCMR = GEO_in_Sospeso
044600160120     c                   z-add     vatAAS        Sosp_AAS
044700160120     c                   z-add     vatLNP        Sosp_LNP
044800160120     c                   z-add     vatNRS        Sosp_NRS
044900160120     c                   z-add     vatNSP        Sosp_NSP
045000160120     c                   eval      IN_ATTESA  = 'S'
045100160120     c                   leaveSR
045200160120     c                   end
045300160120      *--
045400130619     c                   z-add     vatLNP        LineaP
045500110930     c                   end
045600110930      **
045700110930      **  se non ancora confermato da Cliente
045800130619     c                   if        LineaP = 0
045900110930     C                   eval        vatTRC = 'E'
046000110930     C                   eval        vatNOT = i1PARCEL
046100110930     C     Kvat4E        setll     fivat02l
046200110930     C     vatTRC        reade     fivat02l
046300110930     c                   IF        not %EoF(fivat02l) AND
046400110930     C                             I1PARCEL = %SUBST(vatNOT:1:14)
046500130619     c                   z-add     vatLNP        LineaP
046600110930     c                   end
046700130619     c                   end
046800160120      **
046900110930      **  se già confermato
047000130619     c                   if        LineaP = 0
047100110930     C                   eval        AR4TRC = 'I'
047200110930     C                   eval        AR4N30 = i1PARCEL
047300110930     C     Kar4I         setll     fiar403l
047400110930     C     ar4TRC        reade     fiar403l
047500110930     c                   IF        not %EoF(fiar403l) AND
047600110930     C                             I1PARCEL = %SUBST(AR4N30:1:14)
047700141103     c********           z-add     ar4LNP        LineaP
047800141103      ***
047900141103      *  controlla se su BLP sono passati più di tot.giorni (tab.3I) oppure
048000141103      *  se deve essere invece considerato il parcel veramente doppio.
048100141103     c     kblp          chain     fnblp01l
048200141103     c                   if        %Found(fnblp01l)
048300141103     C                   Z-ADD     BLPMGS        Data_BLP          8 0
048400141103     C                   MOVEL     BLPAAS        Data_BLP
048500141103    5C     Data_BLP      IFGE      DateuMENOgg
048600141103     c     blpcca        andne     '7'
048700141103     c                   z-add     blpLNP        LineaP
048800141103     c                   end
048900141103     c                   end
049000141103      ***
049100111003     c                   end
049200130619     c                   end
049300130619      *
049400130619      *   controlla che sia DPD con il Network e non con la linea
049500130619     c                   if        LineaP > 0
049600130619     c     LineaP        chain     azorg01l
049700130619     c                   if        %Found(azorg01l)
049800130619     c                   eval      OG143 = orgDE3
049900130619     c                   if         §OGNTW = 'DPD'
050000130619     c                   eval      GIA_PRESENTE = 'S'
050100130619     c                   end
050200130619     c                   endif
050300130619     c                   end
050400130619      **
050500110930     c                   endSR
050600060612      * ?------------------------------------------------------------------ */
050700060612      *?    Decodifica record a campi variabili
050800060612      * ?------------------------------------------------------------------ */
050900060612     c     Decod_Record  Begsr
051000060612
051100060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
051200060612     c                   movel(p)  VINDTA        dati
051300151215     c     riprova       tag
051400110930     c                   EVAL      err_bloccante =' '
051500120305      **
051600120305      * ?              /*---------------------- */
051700141103     c                   exsr      se_Amp_chr
051800120305      * ?              /*---------------------- */
051900141103      **
052000141103      * ?              /*---------------------- */
052100141103     c                   exsr      se_doppioApice
052200141103      * ?              /*---------------------- */
052300060612      **
052400060612      * ?              /*---------------------- */
052500060612     c                   exsr      Split_Rec
052600060612      * ?              /*---------------------- */
052700060612      **
052800060612      **  Records di Intestazione e di fine Msg
052900060612     c                   if        %subst(dati:1:1) ='#'
053000060612      * ?              /*---------------------- */
053100060612     c                   exsr      Rec_Speciali
053200060612      * ?              /*---------------------- */
053300060612     c                   Else
053400140827      **
053500140827      * spostata qua la pulizia del msg di errore da riportare sul vinMSG
053600140827     c                   clear                   Msg_Err
053700140827      ** controlla che lo split del record sia corretto testando la presenza del
053800140828      ** nome BARTOLINI sul campo (20) della schiera.
053900140827      * ?              /*---------------------- */
054000140828     c                   exsr      xDISALLineata
054100140827      * ?              /*---------------------- */
054200060612      ** conta le righe
054300140827     c                   If        salta_record = *blank
054400151215     c                             and RITRADURRE = *blank
054500060612      * ?              /*---------------------- */
054600060612     c                   exsr      Rec_Dettaglio
054700060612      * ?              /*---------------------- */
054800140827     c                   Endif
054900060612     c                   End
055000060612      **
055100151215     c                   if        RITRADURRE = 'S'
055200151215     c                   goto      riprova
055300151215     c                   Endif
055400151215      **
055500060612     c                   Endsr
055600060612      * ?------------------------------------------------------------------ */
055700060612      *?      Record di Testata e di Fine Msg
055800060612      * ?------------------------------------------------------------------ */
055900060612     c     Rec_Speciali  Begsr
056000060612      **
056100060612      * ?  Prima riga
056200060612     c     'FILE'        SCAN      dati:2        position
056300060612     c                   if        %Found and position < 5
056400140828     C                   eval      Identifica_file = dati
056500060612     c                   exsr      Rec_UNO
056600080917     C                   move      'S'           Riga_iNIZIo
056700060612     c                   ElSe
056800060612     c                   End
056900060612      **
057000060612      * ?  Seconda riga (testate dei campi)
057100060612     c     'DEF'         SCAN      dati:2        position
057200060612     c                   if        %Found and position < 5
057300060612     c                   exsr      Rec_DUE
057400060612     c                   End
057500060612      **
057600060612      * ?  Ultima riga
057700060612     c     'END'         SCAN      dati:2        position
057800060612     c                   if        %Found and position < 5
057900060612     c                   exsr      Rec_LAST
058000080917     C                   move      'S'           Riga_fINe
058100060612     c                   End
058200060612      **
058300060612     c                   Endsr
058400060612      * ?------------------------------------------------------------------ */
058500060612      *?      Primo Record
058600060612      * ?------------------------------------------------------------------ */
058700060612     c     Rec_UNO       Begsr
058800060612      **
058900060612      ** esempio di record :
059000060612      * ?   #FILE;depot0016;N00;20050916;093045;4711;
059100060612      **
059200060612      **  Inizializza i campi della trasmissione
059300060612     C                   eval      Depot_Trs_UNO = *blank
059400060612     C                   eval      User_Trs_UNO  = *blank
059500060612     C                   eval      Data_Trs_UNO  = 0
059600060612     C                   eval      Time_Trs_UNO  = 0
059700060612     C                   eval      Num_Trs_UNO   = 0
059800060612
059900060612      **   Memorizza il numero della trasm. da confrontare con l'ultimo record.
060000060612      **     Tutto il dettaglio deve essere compreso entro questi 2 records
060100060612      **    altrimenti il msg.non è completo e bisogna farsi rimandare il file.
060200060612
060300060612      * ? Nel 2° campo di testata c'è il Dispatch Depot
060400060612     C                   z-add     2             sk
060500060612     c                   eval      Depot_Trs_UNO= %subst(Dato_sk(sk):1:+
060600060612     c                             LunDato_sk(sk))
060700060612
060800060612      * ? Nel 3° campo di testata c'è lo USER in Delis
060900060612     C                   z-add     3             sk
061000060612     c                   eval      User_Trs_UNO  = %subst(Dato_sk(sk):1:+
061100060612     c                             LunDato_sk(sk))
061200060612
061300060612      * ? Nel 4° campo di testata c'è la data trasmissione
061400060612     C                   z-add     4             sk
061500060612     c                   if        DatoNum_sk(sk) = 'S'
061600060612     c                   eval      Data_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
061700060612     c                   End
061800060612
061900060612      * ? Nel 5° campo di testata c'è il time di trasmissione
062000060612     C                   z-add     5             sk
062100060612     c                   if        DatoNum_sk(sk) = 'S'
062200060612     c                   eval      Time_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
062300060612     c                   End
062400060612
062500060612      * ? Nel 6° campo di testata c'è il nr.seriale di trasmissione
062600060612     C                   z-add     6             sk
062700060612     c                   if        DatoNum_sk(sk) = 'S'
062800060612     c                   eval      num_trs_UNO = atoi(%TRIM(Dato_sk(sk)))
062900060612     c                   End
063000060612      **
063100060612     c                   Endsr
063200060612      * ?------------------------------------------------------------------ */
063300060612      *?      Secondo Record
063400060612      * ?------------------------------------------------------------------ */
063500060612     c     Rec_DUE       Begsr
063600060612      **
063700060612      ** esempio di record :
063800060612      * ?   #DEF;IGATEAVIS;SDEPOT;DATE;TIME;PARCELNO;SERVICE;RDEPOT;DNAME1;DNAME2;DCONT....
063900060612      **
064000060612      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
064100060612     C                   clear                   Testa_Sk
064200060612
064300060612      * ? Dal 3° campo troviamo i nomi delle intestazioni dei campi C.Request
064400060612     C     3             do        50            sk
064500060612     c                   eval      Testa_Sk(sk-2) = %subst(Dato_sk(sk):1:+
064600060612     c                             LunDato_sk(sk))
064700060612     c                   enddo
064800060612      **
064900060612     c                   Endsr
065000060612      * ?------------------------------------------------------------------ */
065100060612      *?      Ultimo  Record
065200060612      * ?------------------------------------------------------------------ */
065300060612     c     Rec_LAST      Begsr
065400060612      **
065500060612      ** esempio di record :
065600060612      * ?   #END;4711;
065700060612      **
065800060612      **  Inizializza i campi della trasmissione
065900060612     C                   eval      Num_Trs_fin = 0
066000060612      **
066100060612      **  Nell'ultimo record di chiusura deve esserci lo stesso numero di trasmissione
066200060612      **  impostato nel primo record.
066300060612      **
066400060612      * ? Nel 2° campo di fine trsm. c'è il nr.seriale di trasmissione
066500060612     C                   z-add     2             sk
066600060612     c                   if        DatoNum_sk(sk) = 'S'
066700060612     c                   eval      num_trs_fin = atoi(%TRIM(Dato_sk(sk)))
066800060612     c                   End
066900141103      **
067000141103     c                   Endsr
067100141103      * ?------------------------------------------------------------------ */
067200141103      *?    Se ricevuto &amp; va sostituito con 5 blanks
067300141103      * ?------------------------------------------------------------------ */
067400141103     c     se_amp_chr    Begsr
067500141103      **
067600141103      **   se il record contiene "&amp;"  è la traduzione di un carattere estero accentato
067700141103      **   va sostituito con 4 blanks
067800141103     c                   z-add     1             amp               3 0
067900141103     c                   z-add     0             da_amp            3 0
068000151215     c     '&amp;'       scan      dati:amp      da_amp
068100151215     c*****'&amp'        scan      dati:amp      da_amp
068200141103     c                   if        %Found
068300151215     c                   eval      %subst(dati:da_amp:5) = *blanks
068400151215     c******             eval      %subst(dati:da_amp:4) = *blanks
068500141103     c                   end
068600141103      **
068700060612     c                   Endsr
068800060612      * ?------------------------------------------------------------------ */
068900120305      *?    Problema di caratteri dell'est o nordici     racchiusi fra 2 doppi apici
069000060612      * ?------------------------------------------------------------------ */
069100120305     c     se_DoppioApiceBegsr
069200060612      **
069300160919      ** AL MOMENTO SOSTITUISCO LA VECCHIA REGOLA SOTTO modificando solo in modo che il "
069400160919      ** venga sostituito con *blank.
069500160919     c                   eval      dati = %xlate('"':' ':dati)
069600160919     c                   leavesr
069700160919      **-------------------------------------
069800160919      **
069900160919      **
070000120305      **   contengono una traduzione del carattere che finisce con (;) es.: [&amp;] x identificare
070100120305      **   il carattere (&)...... il problema che ci si presenta è il (;) che viene interpretato
070200120305      **   come separatore di campo mentre invece fa solo parte della traduzione de carattere
070300120305      **   ASCII utilizzato al nord o est Europa.
070400120305     c                   clear                   apici_2           1
070500120305     c                   eval      apici_2 = '"'
070600120305      * cerca di individuare se c'è una dicitura che è messa fra (")
070700120305      *  considera l'estremo sinistro
070800120305     c                   z-add     1             dove              3 0
070900120305      *****
071000120305     c     ritenta       tag
071100120305      *****
071200120305     c     apici_2       scan      dati:dove     da_dove           3 0
071300120305     c                   if        %Found
071400120305      *  considera l'estremo destro
071500120305     c     da_dove       add       1             dove
071600120305     c     apici_2       scan      dati:dove     a_dove            3 0
071700120305      *  se esiste anche un estremo destro controlla se in mezzo c'è un (;)
071800120305      *  da eliminare .
071900120305     c                   if        %Found
072000120305     c                   clear                   punto_virgola     1
072100120305     c                   eval      punto_virgola = ';'
072200120305     c     da_dove       add       1             dove
072300120305     c     punto_Virgola scan      dati:dove     in_mezzo          3 0
072400120305      * se il punto e virgola si trova in mezzo ai doppi apici lo deve sostituire
072500120305      * con un blank
072600120305     c                   if        in_mezzo < a_dove
072700120305     c                   eval      %subst(dati:in_mezzo:1) = ' '
072800120305     c     a_dove        add       1             dove
072900120305     c                   goto      ritenta
073000120305     c                   end
073100120305     c                   end
073200120305     c                   end
073300120305      **
073400120305     c                   Endsr
073500140827      * ?------------------------------------------------------------------ */
073600140827      *?      Controllo preventivo di traduzione corretta della riga
073700140827      * ?------------------------------------------------------------------ */
073800140828     c     xDISALLINEAta Begsr
073900140827      **
074000140827     c                   clear                   salta_record      1
074100151215     c                   clear                   RITRADURRE        1
074200151215      **
074300151215      **   prova a sistemare il record andando ad aggiungere il (;) dove
074400151215      **    probabilmente era stato tolto.
074500151215     c                   if              Dato_sk(19)  = 'BARTOLINI'
074600151215     c                   eval      %subst(dati:da_amp:1) = ';'
074700151215     c                   eval      RITRADURRE = 'S'
074800151215     c                   LEAVESR
074900151215     c                   end
075000140827      **
075100140828      **  Se il nome BARTOLINI è al posto giusto significa che i campi precedenti
075200140828      **   si presume siano nei campi correttamente separati dai (;) e che DPD
075300140828      **    abbia scritto bene il RECORD.
075400140828      **   Purtroppo NON è possibile prevedere altri tipi di errori fatti casualmente
075500140828      **    dai vari DEPOT sui records.
075600140827      **
075700140828     c                   if              Dato_sk(20) <> 'BARTOLINI'
075800140827      **
075900140828     c                   eval      Identifica_record = dati
076000140827     c                   eval      salta_record = 'S'
076100140827      * ?-----> Errore
076200140827     c                   eval      tipo_error = 5
076300140827     c                   move      'S'           err_bloccante
076400140827     c                   eval      non_completamente_tradotto = 'S'
076500140827     c                   exsr      Field_Error
076600140827      **
076700140827     c                   End
076800140827      **
076900140827     c                   Endsr
077000120305      * ?------------------------------------------------------------------ */
077100120305      *?      Riga Bolla Import da controllare e tradurre
077200120305      * ?------------------------------------------------------------------ */
077300120305     c     Rec_Dettaglio Begsr
077400120305      **
077500060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
077600060612      **
077700060612     c                   clear                   TIdp100
077800140827     c******                   clear                   Msg_Err
077900060614      **
078000060612      * ? Nel 1° campo : HUB DPD che invia i dati    (Oblligatorio)  SDEPOT
078100060612      **                                                             HUB DPD D'INVIO
078200060612     C                   z-add     1             sk
078300060612     c                   If        LunDato_sk(sk) =  0
078400060612      * ?-----> Errore
078500060614     c                   eval      tipo_error = 1
078600060614     c                   exsr      Field_Error
078700060612     c                   Else
078800060612     c                   eval      i1SDEPOT   = %subst(Dato_sk(sk):1:+
078900060612     c                                              LunDato_sk(sk))
079000060612     c                   End
079100060612      **
079200060612      * ? Nel 2° campo : Data di Trasmissione DPD    (Oblligatorio)  DATE
079300060612      **                                                             DATA TRASM.
079400060612     C                   add       1             sk
079500060612     c                   If        DatoNum_sk(sk) = 'N' or
079600060612     c                             LunDato_sk(sk) =  0
079700060612      * ?-----> Errore
079800060614     c                   eval      tipo_error = 2
079900060614     c                   exsr      Field_Error
080000060612     c                   Else
080100060612     c                   eval      i1DATE   = atoi(%TRIM(Dato_sk(sk)))
080200060612     c                   End
080300060612      **
080400060612      * ? Nel 3° campo : Orario di Trasmissione DPD                  TIME
080500060612      **                                                             ORA TRASM.
080600060612     C                   add       1             sk
080700060612     c                   If        DatoNum_sk(sk) = 'N' and
080800060612     c                             LunDato_sk(sk) >  0
080900060612      * ?-----> Errore
081000060614     c                   eval      tipo_error = 3
081100060614     c                   exsr      Field_Error
081200060612     c                   Else
081300060612     c                   eval      i1TIME   = atoi(%TRIM(Dato_sk(sk)))
081400060612     c                   End
081500060612      **
081600060612      * ? Nel 4° campo : Numero di Parcel            (Oblligatorio)  PARCELNO
081700060612      **                                                             NR.PARCEL DPD
081800060612     C                   add       1             sk
081900060612     c                   If        LunDato_sk(sk) =  0
082000060612      * ?-----> Errore
082100060614     c                   eval      tipo_error = 1
082200060614     c                   exsr      Field_Error
082300060612     c                   Else
082400060612     c                   eval      i1PARCEL = %subst(Dato_sk(sk):1:+
082500060612     c                                              LunDato_sk(sk))
082600060621      ** controlla lunghezza del Parcel
082700060621     c                   clear                   Lung_Parcel
082800060621     c                   eval      Lung_Parcel = %Len(%trim(i1PARCEL))
082900060612     c                   End
083000060612      **
083100060612      * ? Nel 5° campo : Codice di Servizio          (Oblligatorio)  SERVICE
083200060612      **                                                             SERVICE CODE
083300060612     C                   add       1             sk
083400060612     c                   If        DatoNum_sk(sk) = 'N' or
083500060612     c                             LunDato_sk(sk) =  0
083600060612      * ?-----> Errore
083700060614     c                   eval      tipo_error = 2
083800060614     c                   exsr      Field_Error
083900130819     c                   eval      i1SERVICE  = Normal_Parcel
084000060612     c                   Else
084100060612     c                   eval      i1SERVICE  = atoi(%TRIM(Dato_sk(sk)))
084200060612     c                   End
084300060612      **
084400060612      * ? Nel 6° campo : HUB Bartolini               (Oblligatorio)  RDEPOT
084500060612      **                                                             HUB Bartolini
084600060612     C                   add       1             sk
084700060612     c                   If        LunDato_sk(sk) =  0
084800060612      * ?-----> Errore
084900060614     c                   eval      tipo_error = 1
085000060614     c                   exsr      Field_Error
085100060612     c                   Else
085200060612     c                   eval      i1RDEPOT   = %subst(Dato_sk(sk):1:+
085300060612     c                                              LunDato_sk(sk))
085400060612     c                   End
085500060612      **
085600060612      * ? Nel 7° campo : Nome Destinatario           (Oblligatorio)  DNAME1
085700060612      **                                                             NOME DESTINATARIO
085800060612     C                   add       1             sk
085900060612     c                   If        LunDato_sk(sk) =  0
086000060612      * ?-----> Errore
086100060614     c                   eval      tipo_error = 1
086200060614     c                   exsr      Field_Error
086300060612     c                   Else
086400060612     c                   eval      i1DNAME1   = %subst(Dato_sk(sk):1:+
086500060612     c                                              LunDato_sk(sk))
086600060612     c                   End
086700060612      **
086800060612      * ? Nel 8° campo : 2° Nome Destinatario                        DNAME2
086900060612      **                                                             NOME 2 DESTINATARIO
087000060612     C                   add       1             sk
087100060612     c                   If        LunDato_sk(sk) >  0
087200060612     c                   eval      i1DNAME2   = %subst(Dato_sk(sk):1:+
087300060612     c                                              LunDato_sk(sk))
087400060612     c                   End
087500060612      **
087600060612      * ? Nel 9° campo : Nome Contatto Destinatario                  DCONTACT
087700060612      **                                                             NOME CONTATTO
087800060612     C                   add       1             sk
087900060612     c                   If        LunDato_sk(sk) >  0
088000060612     c                   eval      i1DCONTACT = %subst(Dato_sk(sk):1:+
088100060612     c                                              LunDato_sk(sk))
088200060612     c                   End
088300060612      **
088400060612      * ? Nel 10° campo : Indirizzo Ordinante        (Oblligatorio)  DSTREET
088500060612      **                                                             INDIRIZZO DESTINATARIO
088600060612     C                   add       1             sk
088700060612     c                   If        LunDato_sk(sk) =  0
088800060612      * ?-----> Errore
088900060614     c                   eval      tipo_error = 1
089000060614     c                   exsr      Field_Error
089100060612     c                   Else
089200060612     c                   eval      i1DSTREET  = %subst(Dato_sk(sk):1:+
089300060612     c                                              LunDato_sk(sk))
089400060612     c                   End
089500060612      **
089600060612      * ? Nel 11° campo : Nazione Destinatario       (Oblligatorio)  DCOUNTRYN
089700060612      **                                                             NAZIONE DESTINATARIO
089800060612     C                   add       1             sk
089900060612     c                   If        DatoNum_sk(sk) = 'N' or
090000060612     c                             LunDato_sk(sk) =  0
090100060612      * ?-----> Errore
090200060614     c                   eval      tipo_error = 2
090300060614     c                   exsr      Field_Error
090400060612     c                   Else
090500060612     c                   eval      i1DCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
090600060612     c                   End
090700060612      **
090800060612      * ? Nel 12° campo : Cod.Postale Destinatario   (Oblligatorio)  DPOSTAL
090900060612      **                                                             CAP ORDINANTE
091000060612     C                   add       1             sk
091100060612     c                   If        LunDato_sk(sk) =  0
091200060612      * ?-----> Errore
091300060614     c                   eval      tipo_error = 1
091400060614     c                   exsr      Field_Error
091500060612     c                   Else
091600060612     c                   eval      i1DPOSTAL  = %subst(Dato_sk(sk):1:+
091700060612     c                                              LunDato_sk(sk))
091800060612     c                   End
091900060612      **
092000060612      * ? Nel 13° campo : Città Destinatario         (Oblligatorio)  DCITY
092100060612      **                                                             CITTÀ DESTINATARIO
092200060612     C                   add       1             sk
092300060612     c                   If        LunDato_sk(sk) =  0
092400060612      * ?-----> Errore
092500060614     c                   eval      tipo_error = 1
092600060614     c                   exsr      Field_Error
092700060612     c                   Else
092800060612     c                   eval      i1DCITY    = %subst(Dato_sk(sk):1:+
092900060612     c                                              LunDato_sk(sk))
093000060612     c                   End
093100060612      **
093200060612      * ? Nel 14° campo : Provincia                                  DPROVINCE
093300060612      **                                                             PROVINCIA DESTINATARIO
093400060612     C                   add       1             sk
093500060612     c                   If        LunDato_sk(sk) >  0
093600060612     c                   eval      i1DPROV = %subst(Dato_sk(sk):1:+
093700060612     c                                              LunDato_sk(sk))
093800060612     c                   End
093900060612      **
094000060612      * ? Nel 15° campo : Cod ISO NAZIONE Provenienza                TCOUNTRYN
094100060612      **                                                             PAESE DI PROVENIENZA
094200060612     C                   add       1             sk
094300060612     c                   If        DatoNum_sk(sk) = 'N' and
094400060612     c                             LunDato_sk(sk) >  0
094500060612      * ?-----> Errore
094600060614     c                   eval      tipo_error = 3
094700060614     c                   exsr      Field_Error
094800060612     c                   Else
094900060612     c                   eval      i1TCOUNTRY =  atoi(%TRIM(Dato_sk(sk)))
095000060612     c                   End
095100060612      **
095200060612      * ? Nel 16° campo : Cod.Postale Destinatario di Provenienza    TPOSTAL
095300060612      **                                                             CAP DI PROVENIENZA
095400060612     C                   add       1             sk
095500060612     c                   If        LunDato_sk(sk) >  0
095600060612     c                   eval      i1TPOSTAL  = %subst(Dato_sk(sk):1:+
095700060612     c                                              LunDato_sk(sk))
095800060612     c                   End
095900060612      **
096000060612      * ? Nel 17° campo : Telefono Destinatario                      DPHONE
096100060612      **                                                             TEL DESTINATARIO
096200060612     C                   add       1             sk
096300060612     c                   If        LunDato_sk(sk) >  0
096400060612     c                   eval      i1DPHONE  = %subst(Dato_sk(sk):1:+
096500060612     c                                              LunDato_sk(sk))
096600060612     c                   End
096700060612      **
096800060612      * ? Nel 18° campo : FAX      Destinatario                      DFAX
096900060612      **                                                             TEL DESTINATARIO
097000060612     C                   add       1             sk
097100060612     c                   If        LunDato_sk(sk) >  0
097200060612     c                   eval      i1DFAX   = %subst(Dato_sk(sk):1:+
097300060612     c                                              LunDato_sk(sk))
097400060612     c                   End
097500060612      **
097600060612      * ? Nel 19° campo : E-MAIL   Destinatario                      DEMAIL
097700060612      **                                                             E-MAIL DESTINATARIO
097800060612     C                   add       1             sk
097900060612     c                   If        LunDato_sk(sk) >  0
098000060612     c                   eval      i1DEMAIL = %subst(Dato_sk(sk):1:+
098100060612     c                                              LunDato_sk(sk))
098200060612     c                   End
098300060612      **
098400060612      * ? Nel 20° campo : AZIENDA                    (Oblligatorio)  COMPANY
098500060612      **                                                             ID.AZIENDA
098600060612     C                   add       1             sk
098700060612     c                   If        LunDato_sk(sk) =  0
098800060612      * ?-----> Errore
098900060614     c                   eval      tipo_error = 1
099000060614     c                   exsr      Field_Error
099100060612     c                   Else
099200060612     c                   eval      i1COMPANY  = %subst(Dato_sk(sk):1:+
099300060612     c                                              LunDato_sk(sk))
099400060612     c                   End
099500060612      **
099600060612      * ? Nel 21° campo : GEOID                                      GEOID
099700060612      **                                                             COD.ASCII 037
099800060621     C                   clear                   i1GEOID_A         1
099900060612     C                   add       1             sk
100000060612     c                   If        DatoNum_sk(sk) = 'N' and
100100060612     c                             LunDato_sk(sk) >  0
100200060612      * ?-----> Errore
100300060614     c                   eval      tipo_error = 3
100400060614     c                   exsr      Field_Error
100500060612     c                   Else
100600060612     c                   eval      i1GEOID =  atoi(%TRIM(Dato_sk(sk)))
100700060612
100800060612     C                   move(p)   i1GEOID       §xteAdec
100900060612      **   Convertitore
101000060612     c                   call      'XTOEBCDR'
101100060612     c                   parm                    xtoEBCDds
101200060612     C                   IF        §XTEERR <> *blanks
101300060612      **  Errore di Conversione
101400060612      * ?-----> Errore
101500060614     c                   eval      tipo_error = 4
101600060614     c                   exsr      Field_Error
101700060718     C                   move      '%'           i1GEOID_A         1
101800060612     C                   ELSE
101900060718      * se il valore era "000" ossia non era arrivato da DPD impostiamo default "%"
102000060718     c                   if        §xteAchr = 'NUL'
102100060718     C                   move      '%'           i1GEOID_A
102200060718     c                   else
102300060612     C                   move      §xteAchr      i1GEOID_A
102400060718     c                   end
102500060612     C                   ENDIF
102600060612     c                   End
102700060612      **
102800060612      * ? Nel 22° campo : Numero Mezzo CMR           (Oblligatorio)  REMARK
102900060612      **                                                             LORRY NUMBER
103000060612     C                   add       1             sk
103100060612     c                   If        LunDato_sk(sk) =  0
103200060612      * ?-----> Errore
103300060614     c                   eval      tipo_error = 1
103400060614     c                   exsr      Field_Error
103500060612     c                   Else
103600060612     c                   eval      i1REMARK   = %subst(Dato_sk(sk):1:+
103700060612     c                                              LunDato_sk(sk))
103800100105      *
103900100105      * se non c'è il CMR imposta la HUB di provenienza
104000100105     c                   if        i1REMARK = *blank
104100100105     c                   Eval      i1REMARK = %Trim(i1sdepot)
104200100105     c                   End
104300100105      *
104400060612     c                   End
104500060612      **
104600060612      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDP100W
104700060612      **
104800060612     c                   Endsr
104900060614      * ?------------------------------------------------------------------ */
105000060614      *?      Campi in errore
105100060614      * ?------------------------------------------------------------------ */
105200060614     c     Field_Error   Begsr
105300060614      **
105400060614     C                   eval      se_errore   = 'S'
105500060614      * msg video o in posta
105600060614     c                   If        Msg_Err = *blank
105700060614     c
105800060614     c                   Select
105900060614     c
106000060614     c                   When      Tipo_Error = 1
106100060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
106200060614     C                             ' >> Obblig.- Manca Dato !!'
106300060614     c
106400060614     c                   When      Tipo_Error = 2
106500060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
106600060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
106700060614     c
106800060614     c                   When      Tipo_Error = 3
106900060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
107000060614     C                             ' >> NON Numerico  !!'
107100060614     c
107200060614     c                   When      Tipo_Error = 4
107300060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
107400060614     C                             ' >> Conversione non riuscita !!'
107500140827     c
107600140827      * non essendo un campo ma tutta la riga l'indice di skiera è sposizionato
107700140827      * NON deve quindi eseguire EVAL del "Msg_Vin_80" seguente xché si
107800140827      * spaccherebbe per indice di schiera NON valido.
107900140827     c                   When      Tipo_Error = 5
108000140827     C                   eval      Msg_Err = 'Campi riga sposizionati >>' +
108100140827     C                             ' Spedizione NON tradotta !!'
108200140827     C                   eval      Msg_Vin_80 = Msg_Err
108300060614     c                   EndSL
108400060614     c
108500060614     c                   End
108600060614      * Errore su VINMSG
108700140827     c                   if        Tipo_Error <> 5
108800060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
108900060614     c                                          + %trim(Testa_sk(sk))
109000140827     c                   End
109100060614      **
109200060614     c                   Endsr
109300060612      * ?------------------------------------------------------------------ */
109400060612      *?      Suddivide i campi della riga con carattere divisorio (;)
109500060612      * ?------------------------------------------------------------------ */
109600060612     c     Split_Rec     Begsr
109700060612      **
109800060612     c                   clear                   Nr_campi
109900060612     c                   clear                   Campi_Record
110000060612     c                   clear                   Campi_Lunghi
110100060612     c                   clear                   Campi_Numerici
110200060612     c                   clear                   Campi_Decimali
110300060612     c                   movel     ';'           Separa_campi
110400060612     c                   movel     '.'           Decimal_Separ
110500060612      *
110600060612     c                   call      'FIEU00R'
110700060612      * ?     Input
110800060612     c                   parm                    Separa_campi
110900060612     c                   parm                    Decimal_Separ
111000060612     c                   parm                    dati
111100060612      * ?     Output  su schiere di 100 elementi
111200060612     c                   parm                    Nr_campi
111300060612     c                   parm                    Campi_Record                   * i dati
111400060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
111500060612     c                   parm                    Campi_Numerici                 * se numerici
111600060612     c                   parm                    Campi_Decimali                 * quanti decimali
111700060612      **
111800060612     c                   Endsr
111900160120      * ?================================================================== */
112000160120     C*?  ERA GIA' su EDIVAB perchè scritto da GEODATA
112100160120      * ?================================================================== */
112200160120     C     UPD_DA_GEO    BEGSR
112300160120      *
112400160120     c                   clear                   new_CMR          35
112500160120      *
112600160120     c                   eval      new_CMR  = %trim(i1REMARK)+'_('+
112700160120     c                             %trim(i1RDEPOT) + ')_'+
112800160120     c                             %trim(Depot_TRS_UNO)
112900160120      *  Solo se ci sta aggiungo la data
113000160120     c                   clear                   lungo_size        3 0
113100160120     c                   eval      lungo_size = %len(%Trim(new_CMR))
113200160120     c                   if        lungo_size < 30
113300160120     c                   z-add     DATEU         DATA4             4 0
113400160120     c                   EVAL      new_CMR = %trim(new_CMR) + '_' +
113500160120     c                             %trim(%editw(DATA4: '0 /  '))
113600160120     c                   end
113700160120      *
113800160120      *  per chiave bolla aggiorna la bolla in SOSPENZIONE che adesso
113900160120      *  risulta partita con l'IGATEAVIS ricevuto (Manifest)
114000160120     C     KSosp_EDIVA   setll     EDIVAT3L
114100160120     C     KSosp_EDIVA   reade     EDIVAT3L
114200160120     c                   DOw       not %EoF(EDIVAT3L)
114300160120     c                   eval      vatCMR  =  new_CMR
114400160120     c                   Update    EDIVAT00
114500160120     C     KSosp_EDIVA   reade     EDIVAT3L
114600160120     c                   EndDO
114700160120      *
114800160120      *  poi allinea anche la testata bolla del VAB
114900160120     C     KSosp_EDIVA   chain     EDIVAB2L
115000160120     c                   if        %Found(EDIVAB2L)
115100160120     c                   eval      vabCMR  =  new_CMR
115200160208      *** Provenienza
115300160208     c                   movel     'X'           VABscl
115400160120     c                   Update    EDIVAB2
115500160120     c                   End
115600160120      *
115700160120     c                   Endsr
115800060612      * ?================================================================== */
115900060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
116000060612      * ?================================================================== */
116100060612     C     WRT_VAB_VAT   BEGSR
116200060614      *
116300060612      * Clearizzo EDiVAB in quanto ha gli stessi campi di FiVAB più
116400060612      *  alcuni specifici
116500060612     c                   clear                   edivab00
116600060612     c                   clear                   fivab000
116700060621     c                   clear                   err_bloccante     1
116800100105     c                   clear                   Sped_RESO         1
116900060614      *
117000060612      **?------------------------------------------------------------------ */
117100060612      *   In base alla HUB di arrivo determino la Filiale di gestione
117200060612      *?   Il tutto è pilotato dalla tabella "DEP"
117300060612      **?------------------------------------------------------------------ */
117400060612      * ?Dal Depot DPD prendo il relativo cliente e LNP se su tab.:DEP
117500060612     c                   z-add     1             xx
117600060612     c                   clear                   su_EDIVAB         1
117700091106     c                   movel(P)  i1SDEPOT      W015A
117800091106     c                   movel(P)  W015A         W030A
117900091106     c                   moveL(P)  i1RDEPOT      W015A
118000091106     c                   move      W015A         W030A
118100060913     c                   clear                   depotK            4
118200150604     c                   clear                   codtarVTG         3 0
118300091106      *
118400091106      * completo con HUB di arrivo
118500091106     c     W030A         lookup    DEPK2(xx)                              31
118600091109     c  n31              clear                   W015A
118700091106      * senza HUB di arrivo
118800091106     c  n31              move      W015A         W030A
118900091106     c  n31W030A         lookup    DEPK2(xx)                              31
119000091106      *
119100091106     c                   if        *in31
119200060620      **
119300060913     c                   movel     DepK(xx)      depotK
119400060612     c                   movel     DepC(xx)      vabCCM
119500060612     c                   movel     DepL(xx)      vabLNP
119600060612     c                   movel     DepL(xx)      vabFGS
119700060612     c                   movel     DepM(xx)      vabCTM
119800060612     c                   movel     DepT(xx)      vabCTR
119900150604     c                   movel     DepTvtg(xx)   codtarVTG         3 0
120000060612     c                   movel     DepW(xx)      su_EDIVAB
120100160301      **
120200160301     c                   movel     'LPD'         tbecod
120300160301     c                   movel(p)  vablnp        tbeke1
120400160301     c     KeyTBE        chain     tntbe01l
120500160301     c                   if        %Found(tntbe01l)
120600160301     c                   eval      dLPD = tbeUNI
120700160301     c                   If        §LPDwriIGA = 'N'
120800160301     c                   eval      se_errore ='S'
120900160301     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) + ';' +
121000160301     c                             'Salta write x tab.LPD-> NO IGATEAVIS'
121100160301     c                   leaveSR
121200160301     c                   end
121300160301     c                   else
121400160301     c                   end
121500160301      **
121600060612     c                   else
121700060612      **
121800060612      *?   SE è arrivato sul file un HUB non presente in tabella "DEP" invia messaggio
121900060612      * ?Se è arrivata un Depot/HUB non configurato x la ricezione
122000060612      * ?   AVVISA mediante msg utente e Posta AS400 della mancata ricezione
122100060612      **
122200060614     C                   eval      se_errore   = 'S'
122300060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
122400060614     c                                          + 'TAB:DEP ' + %trim(i1SDEPOT)
122500091106     c                                          + '/' + %trim(i1RDEPOT)
122600091106     c                                          + '_Mancante'
122700060621     c                   move      'S'           err_bloccante
122800120504      *
122900120504      *  Attivato xchè importante inviare un MSG
123000120504      *
123100120504     c                   if        non_inviato_prima = *blank
123200120514     C                   movel     ' '           Msg_Vin_80
123300120514     C                   eval      wrkMsg = Msg_Vin_80
123400120514     C                   eval      wrkMsg = %trim(wrkMSG) + ' inserire su tabel+
123500120514     c                             la DEP (TNTBE00F) il nuovo codice DEPOT DPD +
123600120514     c                             x poter ricevere i dati. Dopo avere inserito+
123700120514     c                              la tabella rieseguire l''UPLOAD di questo f+
123800120514     c                             ile.  +
123900120514     c                             Avvisare   Andrea o Nicola.'
124000120504     C                   exSR      eMail_alert
124100120504     c                   eval      non_inviato_prima = 'S'
124200120504     c                   end
124300120504      *
124400060614     c                   goto      Error_VAB
124500060612     c                   end
124600060612      **?------------------------------------------------------------------ */
124700060612      * ?   ( Il parcel non è + numerico )
124800060612     c
124900060612      * ?il Parcel deve essere impostato nella DS VATE
125000060612      * e viene impostato
125100060612      * fisso il rif.numerico a (1)
125200060710      *  se comunque il Parcel ricevuto è numerico impostiamo nel riferimento
125300060710      *  numerico il nr.Parcel come avveniva prima che diventasse alfa il campo
125400060710     C                   z-add     1             vabrmn
125500060710      **
125600060710     c     digits        check     i1PARCEL      position
125700060710      **
125800060710      **  presenti campi non numerici: (Ossia trovati)
125900060710     c                   iF        %Found
126000060911      * ?  per impostare il riferimento numerico (VABRMN):
126100060710      **    è importante che siano oltre il limite della lunghezza del Parcel
126200060710      **    in tal caso sono solo i blanks alla destra del numero
126300060710     c                   if        Lung_Parcel = 11 and position > 11
126400060710     C                   MOVEL     i1PARCEL      parcel11
126500060710     c                   move      parcel11      vabrmn
126600060710     c                   end
126700060710      **
126800060710     c                   if        Lung_Parcel = 14 and position > 14
126900060710     C                   MOVEL     i1PARCEL      parcel14
127000060710     c                   move      parcel14      vabrmn
127100060710     c                   end
127200060710      **
127300060710     c                   elsE
127400060710      **
127500060710     c                   if        Lung_Parcel = 11
127600060710     C                   MOVEL     i1PARCEL      parcel11
127700060710     c                   move      parcel11      vabrmn
127800060710     c                   end
127900060710     c                   if        Lung_Parcel = 14
128000060710     C                   MOVEL     i1PARCEL      parcel14
128100060710     c                   move      parcel14      vabrmn
128200060710     c                   end
128300060710      **
128400060710     c                   enD
128500150604      **
128600150604      ** imposta il Depot di provenienza x testare se VTG
128700150604     c                   movel     parcel14      Depot04
128800150604     c                   clear                   parcel_VTG        1
128900150604      **
129000150604     C                   movel(p)  'VTG'         cldCOD
129100150604     C                   move      Depot04       cldDEP
129200150604     c     key_VTG       setll     tncld00F
129300150604     c     key_VTG       reade     tncld00F
129400150604     c                   dow       not %EoF(tncld00F)
129500150604     c                   if        dateu >= cldDDA and
129600150604     c                             dateu <= cldADA
129700150604     c                   move      'S'           parcel_VTG
129800150604     c                   leave
129900150604     c                   end
130000150604     c     key_VTG       reade     tncld00F
130100150604     c                   endDO
130200150604      **
130300150604      **  Se è un collo VTG deve avere una tariffa differente
130400150604     c                   if         parcel_VTG = 'S'
130500150604     c                   movel     codtarVTG     vabCTR
130600150604     c                   end
130700060612      * -----------------                  -------------------
130800060612      * ?Numero spedizione
130900060612     C                   EXSR      SUB_keybol
131000060612      *
131100060612      * ?Data  spedizione
131200060612     C                   MOVEL     UMONTH        VABMGS                         = oggi
131300060612     C                   MOVE      Uday          VABMGS                         = oggi
131400060612      *
131500060612      * ?Dati  destinatario
131600060612     c                   movel     i1DNAME1      vabrsd                         Ragione Sociale
131700060612     C                   movel     i1DNAME2      vabrd2                         rag.soc.2
131800060612     c                   movel     i1DSTREET     vabind                         Indirizzo
131900060612     c                   movel     i1DCITY       vablod                         Località
132000060612     c                   movel     i1DPOSTAL     vabcad                         C.A.P.
132100060612     c                   movel     i1DPROV       vabprd                         Provincia
132200070202      * ?Se manca la provincia la calcolo
132300070202     c                   if        vabprd = *blank
132400070202     C                   CLEAR                   TISI95DS
132500070202     C                   MOVEL     vabCAD        I95CAP
132600070202     C                   MOVEL     DATEU         I95DAT
132700070202     C                   MOVEL     '3'           I95TCN
132800070202     C                   CALL      'TISI95R'
132900070202     C                   PARM                    TISI95DS
133000070202      *     Errore
133100070202     C     O95ERR        IFeq      *BLANKS
133200070202     C                   MOVEL     O95PRV        vabPRD
133300070202     C                   END
133400070202      *
133500070202     c                   end
133600060612      * ?Tipo servizio
133700060612     c                   movel     'C'           vabtsp
133800060612      * ?Colli
133900060612     c                   z-add     1             vabncl
134000060612      *
134100060612      * ?Service  Code:  101-Normal Parcel
134200060612      * ?                136-Small Parcel
134300060612      * ?                298-Return Sender
134400060612      * ?                300-Return
134500111110      * ?                327-Normal Parcel B2C
134600111110      * ?                329-Normal Parcel B2C
134700111110      * ?                328-Small  Parcel B2C
134800140409      * ?                365-Tyres
134900140409      * ?                366-Tyres B2C
135000140409      * ?                367-Tyres COD
135100060621     c                   clear                   w015A
135200060612     c                   if        i1SERVICE = 101
135300111110      *******  da eliminare in seguito
135400111110     c                               or
135500111110     c                             i1SERVICE = 327 or i1SERVICE = 329
135600111110      *******
135700060612     c                   eval      W015A = 'PESO01'
135800060612     c                   end
135900141210      *******
136000141210      ** aggiunto per il traffico gomme.
136100141210     c                   if        i1SERVICE = 365
136200141210     c                               or
136300141210     c                             i1SERVICE = 366
136400141210     c                               or
136500141210     c                             i1SERVICE = 367
136600141210     c                   eval      W015A = 'PESO03'
136700141210     c                   end
136800140409      *
136900060612     c                   if        i1SERVICE = 136
137000111110      *******  da eliminare in seguito
137100111110     c                               or
137200111110     c                             i1SERVICE = 328
137300111110      *******
137400060612     c                   eval      W015A = 'PESO02'
137500060612     c                   end
137600130819      ***
137700130819      * se per qualche motivo non passano il codice pacco/peso imposto come
137800130819      *   pacco grande
137900130819     c                   if        W015A = *blank
138000130819     c                   eval      W015A = 'PESO01'
138100130819     c                   end
138200130819      *
138300060612      * ?Peso, ricavato dal tipo servizio del ROUTELABEL
138400060612     c                   z-add     1             xx
138500060621     c                   if        w015a <> *blank
138600060612     c     W015A         lookup    DPPK(xx)                               31
138700060612     c                   If        *in31 = *off
138800060612     c                   eval      W015A = 'PESO'
138900060612     c     W015A         lookup    DPPK(xx)                               31
139000060612     c                   Endif
139100060612     c                   z-add     DPP(xx)       vabPKB
139200161006     c                   z-add     vlb(xx)       vabVLB
139300060621     c                   End
139400060612      *
139500060612      * ?Codice bolla
139600060612     C                   MOVEL     '1'           vabcbo                         NO C/assegno
139700060612      *
139800060612      * ?Se spedizione RESA imposto diversamente il VAB
139900060911      * ?   ma se DPD non invia correttamente il service Code
140000060911      * ?   identifichiamo il reso dal Depot Bartolini sul Parcel
140100060911      **
140200060911     c                   if        Lung_Parcel = 11
140300060911     C                   MOVEL     i1PARCEL      DepotParcel
140400060911     c                   end
140500060911     c                   if        Lung_Parcel = 14
140600060911     c                   movel     i1PARCEL      DepotPrc4         4
140700060911     c                   move      DepotPrc4     DepotParcel
140800060911     c                   end
140900060911      **
141000060612     c                   if        i1SERVICE = 298 or
141100060911     c                             i1SERVICE = 300          or
141200060911     c                             (DepotParcel = '844' or DepotParcel = '845')
141300060612     C                   clear                   VABpkb
141400060612     C                   clear                   VABncl
141500060612     C                   eval      VABrd2 = '    ** RESO **'
141600100105     c                   eval      Sped_RESO = 'S'
141700060612     c                   ENDIF
141800060612      *
141900060612      * ?E-Mail destinatario da contattare
142000060612     c                   if        i1DEMAIL <> *blank
142100060612     C                   eval      VABnot = i1DEMAIL
142200060612     c                   ENDIF
142300060612      *
142400060612      * ?FAX destinatario da contattare
142500060612     c                   if        i1DFAX   <> *blank
142600060612     C                   eval      VABnt2 = 'FAX:' + i1DFAX
142700060612     c                   ENDIF
142800060612      *
142900061016      * ?Controlla esistenza del Parcel sul file degli ORM dati a DPD per
143000061016      * ? impostare la bolla in assegnato
143100061016     c                   if        Lung_Parcel = 11
143200061016     C                   MOVEL(p)  parcel11      dpo_Parcel       14
143300061016     c                   end
143400061016     c                   if        Lung_Parcel = 14
143500061016     C                   MOVEL(p)  parcel14      dpo_Parcel
143600061016     c                   end
143700061016      * ?Controlla sul FIDPO x metterlo in assegnato
143800061016     C     dpo_Parcel    chain     fidpo06l
143900061016     c                   if        %Found(fidpo06l)
144000061016      *assegnato
144100061016     C                   MOVEL     '2'           vabcbo
144200061016     c                   else
144300061016      * ?Se non l'ha trovato potrebbe essere un Parcel Taroccato ossia lungo 14
144400061016      * ? ma con 0 e 99 in mezzo quindi ritento l'aggancio.
144500061016     c                   if        Lung_Parcel = 14
144600061016     C                   MOVEL     i1PARCEL      dsparcel
144700061016     c                   if        Parcel0 = '0' and NoveNove ='99'
144800121115      *  NON deve più farlo
144900121115     c                   goto      vecchioPRCL
145000061016      *  Deve reimpostare a 11 il Parcel
145100061016     c                   clear                   dpo_Parcel
145200061016     c                   movel     Depot3        parcel11a        11
145300061016     c                   move      NumParcel     parcel11a
145400061016     C                   MOVEL(p)  parcel11a     dpo_Parcel       14
145500061016     C     dpo_Parcel    chain     fidpo06l
145600061016     c                   if        %Found(fidpo06l)
145700061016      *assegnato
145800061016     C                   MOVEL     '2'           vabcbo
145900061016     c                   end
146000121115     c     vecchioPRCL   tag
146100061016     c                   end
146200061016     c                   end
146300061016      *
146400061016     c                   end
146500061016      *
146600100920      * ?SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
146700100920      * ?in FERMO DEPOSITO.
146800100920     c                   eval      vabFFD = *blank
146900100920     C                   if         vabcbo = '2'  and dpoPOE > 0
147000100920      *
147100100920     C                   z-add     dpoPOE        ORMPOE
147200100920     C                   z-add     dpoNRS        ORMNSR
147300100920     C                   z-add     dpoNOR        ORMNOR
147400100920     C                   z-add     dpoNRV        ORMNRV
147500100920      * ?lo vedo sull'ORM
147600100920     C     KORM          chain     fnORM01l
147700100920     c                   if        %Found(FnOrm01l) and ormffd ='S'
147800100920     c                   eval      vabFFD = 'S'
147900100920     c                   end
148000100920     c                   end
148100101112      *
148200141210     c********** ELIMINATO e GESTITO con SERVICE CODE GOMME e tabella "PESO03"
148300101112      *  Chiodo per la linea 193 solo merce ricevuta dal Depot '0130'
148400101112      *  sono camion di Gomme che devono avere peso medio di 19 Kg invece che 11 Kg
148500101112      *  e volume 0,12 mc anzichè 0,055 mc.
148600141210     c**********         if          vablnp = 193 and i1SDEPOT = '0130'
148700141210     c**********         z-add     19            vabPKB
148800141210     c**********         z-add     0,12          vabVLB
148900141210     c**********         end
149000151030      *** Provenienza
149100151030     c                   movel     'I'           VABscl
149200151113      *
149300151113      * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
149400100920      *
149500151113      * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
149600151113      * ?==============================
149700151113      * ?==============================
149800060612      * ?Scrivo EDiVAB o FiVAB
149900151113      * ?==============================
150000151113      * ?==============================
150100060612     C                   If        su_EDIVAB = 'S'
150200060612      ***
150300091223      *
150400100105      * ?      COMPONE IL CMR per identificare il collo NELLA CASSA/Trasmissione
150500100105     c                   if        Sped_Reso = ' '
150600111010      **********
150700111010     c                   eval      i1REMARK = %trim(i1REMARK)+'_('+
150800111010     c                             %trim(i1RDEPOT) +
150900111010     c                             ')_'+ %trim(Depot_TRS_UNO)
151000111010      *
151100100105      * se trattasi di Reso (Evidenzio)
151200100105     c                   ElseIf    Sped_Reso = 'S'
151300100105     c                   movel(p)  i1REMARK      campo35          35
151400100105     c                   movel(p)  'RESI_'       i1REMARK
151500100107     c                   eval      i1REMARK = %trim(i1REMARK) +
151600100107     c                                        %trim(campo35)
151700100105      *
151800100105     c                   eval      i1REMARK = %trim(i1REMARK) +
151900100105     c                             '_('+ %trim(i1RDEPOT) +
152000100105     c                             ')_'+ %trim(Depot_TRS_UNO) +':' +
152100100105     c                             %trim(%editc(Num_Trs_UNO:'Z'))
152200100105     c                   end
152300090629      *
152400090629      *  Solo se ci sta aggiungo la data
152500090629     C                   move      i1REMARK      VABcmr
152600090629      *
152700090629     c                   clear                   lungo_size        3 0
152800090629     c                   eval      lungo_size = %len(%Trim(VABCMR))
152900090629     c                   if        lungo_size < 30
153000090629     c                   z-add     DATEU         DATA4             4 0
153100090629     c                   EVAL      VABCMR = %trim(VABCMR) + '_' +
153200090629     c                             %trim(%editw(DATA4: '0 /  '))
153300090629     c                   end
153400100315      *
153500060612     c                   z-add     DATEU         VABdcm
153600060612     c                   z-add     DATEU         VABdts
153700060612     c                   movel     WORA          VABhms
153800150908      ***
153900060612     c                   write     EDivab00
154000060612     c                   exsr      wrtvat
154100060612      *
154200060612     c                   ELSE
154300060612      ***
154400060612     c                   write     FiVAB000
154500060612     c                   exsr      wrtvat
154600060612      ***
154700060612     c                   ENDIF
154800060614      ***
154900060614     c                   move      'S'           Almeno_Uno
155000060612      *
155100060614     c     Error_VAB     Endsr
155200060612     C*----------------------------------------------------------------
155300060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
155400060612     C*----------------------------------------------------------------
155500060612     C     WRTVAT        BEGSR
155600060612      *
155700060413      * ? Parcel Nr. --> Riferimento Parcel DPD
155800051219     C                   If        su_EDIVAB = 'S'
155900041215     c                   clear                   edivat00
156000041215     c                   else
156100041215     c                   clear                   fivat000
156200041215     c                   end
156300041215      **
156400041215     C                   movel     VABfgs        VATfgs
156500041215     C                   Z-ADD     VABCCM        VATCCM
156600041215     C                   Z-ADD     VABAAS        VATAAS
156700041215     C                   Z-ADD     VABLNP        VATLNP
156800041215     C                   Z-ADD     VABNRS        VATNRS
156900041215     C                   Z-ADD     VABNSP        VATNSP
157000060413      **
157100060608     C                   MOVEL     'E'           VATTRC
157200060612      **
157300060612      **  su nuovo tipo record (E)
157400060608     c                   clear                   DVate
157500060621      **
157600121115      *  NON deve più farlo
157700121115     c                   goto      vecchioPRCL1
157800060620      * attenzione se si riceve un Parcel "Taroccato" ossia è di 11 ma con 0...99......
157900060620      * si deve riportarlo a 11
158000060620     C                   MOVEL     i1PARCEL      dsparcel
158100060621     c                   clear                   parcel11
158200060620      *  Deve reimpostare a 11 il Parcel
158300060620     c                   if        Parcel0 = '0' and NoveNove ='99'
158400060620     c                   movel     Depot3        parcel11
158500060620     c                   move      NumParcel     parcel11
158600060621     c                   else
158700060621      * se comunque il parcel è arrivato a 11
158800060621     c                   if        Lung_Parcel = 11
158900060621     C                   MOVEL     i1PARCEL      parcel11
159000060621     c                   end
159100060621     c                   end
159200060621      *
159300060621      *  Se invece il parcel è a 11 deve impostare il Check Digit
159400060621     c                   if        parcel11 <> *blank
159500060621     C                   CLEAR                   DPDBRC
159600060621     C                   MOVEL     parcel11      DPDBRC
159700060621     C                   EXSR      M1031
159800060621     C                   movel(p)  DPDBRC        i1PARCEL
159900060621     c                   end
160000121115     c     vecchioPRCL1  tag
160100060621      *
160200060621      *  IMPOSTA la DS VAT(E)
160300060612     C                   MOVEL     i1PARCEL      §VATEPRN
160400060620     C                   move      *zeros        §VATESCD
160500060620     C                   move      i1SERVICE     §VATESCD
160600060612     C                   move(p)   i1GEOID_A     §VATEBID
160700060608     C                   MOVEL     *blank        §VATEROU
160800060620      *
160900060621      *  < DOVE VADO >
161000060620      * attenzione se ci mandano un parcel ancora lungo 11 imposto il DOVE VADO fisso
161100060621      * poichè non viene + passato dal nuovo tracciato.     CHEK DIGIT non serve +
161200060620     c                   if        Lung_Parcel = 11
161300060620     c                   move      '39'          Italia39
161400060620     c                   clear                   ChkDigit
161500060620     c                   movel     VABCAD        CapItalia
161600060620     c                   move      '00'          TipoPacco
161700060620     c                   if        i1SERVICE = 101
161800111110      *******  da eliminare in seguito
161900111110     c                               or
162000111110     c                             i1SERVICE = 327 or i1SERVICE = 329
162100140409      ** aggiunto per il traffico gomme.
162200140409     c                               or
162300140409     c                             i1SERVICE = 365
162400140409     c                               or
162500140409     c                             i1SERVICE = 366
162600140409     c                               or
162700140409     c                             i1SERVICE = 367
162800111110      *******
162900060620     c                   move      '01'          TipoPacco
163000060620     c                   end
163100140409      *
163200060620     c                   if        i1SERVICE = 136
163300111110      *******  da eliminare in seguito
163400111110     c                               or
163500111110     c                             i1SERVICE = 328
163600111110      *******
163700060620     c                   move      '02'          TipoPacco
163800060620     c                   end
163900060620     C                   MOVEL     DoveVado      §VATEROU
164000060620     c                   endIF
164100060608     C                   MOVEL     DVate         VATNOT
164200041215      **
164300051219     C                   If        su_EDIVAB = 'S'
164400041215     C                   MOVEL     VABCMR        VATCMR
164500041215     C                   Z-ADD     VABCNT        VATCNT
164600041215     C                   WRITE     EDiVAT00
164700041215     c                   else
164800041215     C                   WRITE     fiVAT000
164900041215     c                   end
165000060413      *
165100060413      * ? Contatto del Destinatario
165200060612     C                   if        i1DCONTACT <> *Blanks
165300060413     C                   If        su_EDIVAB = 'S'
165400060413     c                   clear                   edivat00
165500060413     c                   else
165600060413     c                   clear                   fivat000
165700060413     c                   end
165800060413      **
165900060413     C                   movel     VABfgs        VATfgs
166000060413     C                   Z-ADD     VABCCM        VATCCM
166100060413     C                   Z-ADD     VABAAS        VATAAS
166200060413     C                   Z-ADD     VABLNP        VATLNP
166300060413     C                   Z-ADD     VABNRS        VATNRS
166400060413     C                   Z-ADD     VABNSP        VATNSP
166500060413      **
166600060413     C                   MOVEL     'A'           VATTRC
166700060612     C                   MOVEL     i1DCONTACT    VATNOT
166800060413      **
166900060413     C                   If        su_EDIVAB = 'S'
167000060413     C                   MOVEL     VABCMR        VATCMR
167100060413     C                   Z-ADD     VABCNT        VATCNT
167200060413     C                   WRITE     EDiVAT00
167300060413     c                   else
167400060413     C                   WRITE     fiVAT000
167500060413     c                   end
167600060413     c                   endIF
167700041215      *
167800060413      * ? Contatto Telefonico
167900060612     C                   if        i1DPHONE   <> *Blanks
168000060413     C                   If        su_EDIVAB = 'S'
168100060413     c                   clear                   edivat00
168200060413     c                   else
168300060413     c                   clear                   fivat000
168400060413     c                   end
168500060413      **
168600060413     C                   movel     VABfgs        VATfgs
168700060413     C                   Z-ADD     VABCCM        VATCCM
168800060413     C                   Z-ADD     VABAAS        VATAAS
168900060413     C                   Z-ADD     VABLNP        VATLNP
169000060413     C                   Z-ADD     VABNRS        VATNRS
169100060413     C                   Z-ADD     VABNSP        VATNSP
169200060413      **
169300060413     C                   MOVEL     'B'           VATTRC
169400060612     C                   MOVEL     i1DPHONE      VATNOT
169500060413      **
169600060413     C                   If        su_EDIVAB = 'S'
169700060413     C                   MOVEL     VABCMR        VATCMR
169800060413     C                   Z-ADD     VABCNT        VATCNT
169900060413     C                   WRITE     EDiVAT00
170000060413     c                   else
170100060413     C                   WRITE     fiVAT000
170200060413     c                   end
170300060413     c                   endIF
170400060413      *
170500060413      *
170600041215     C                   ENDSR
170700971215      *----------------------------------------------------------------
170800971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
170900971215      *----------------------------------------------------------------
171000991206     C     SUB_keybol    BEGSR
171100050112      *
171200050112      **              **------------------**
171300050112      *    deve controllare sulla tabella "3C" se il numero spedizione
171400050112      *     deve essere mantenuto oppure no
171500050112     C                   clear                   Ds3C
171600060620     C                   Z-ADD     1             TBLKUT
171700050112     C                   MOVEL     '3C'          TBLCOD
171800050112     C                   movel(p)  VABccm        TBLKEY
171900050112     C     KTABel        CHAIN     TABEL00F                           30
172000050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
172100050112     C                   MOVEL     TBLUNI        Ds3C
172200050112     C                   END
172300050112      *
172400050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
172500050112     c                   if        §3CFsp <> 'S'
172600050112      *
172700050112     C* NSP => Stacco un numeratore da AZNUM
172800050112     c                   movel     kpjbu         svkpjbu
172900050112     c                   clear                   kpjbu
173000050112     C                   clear                   TRUL33DS
173100050112     C                   eval      I33OPE = *zeros
173200050112     C                   eval      I33CNU = 302
173300050112     C                   eval      I33NUM = 1
173400050112     C                   movel     TRUL33DS      KPJBU
173500050112     C                   call      'TRUL33R'
173600050112     C                   parm                    KPJBA
173700050112     C                   movel     KPJBU         TRUL33DS
173800050112     C                   if        O33ERR = *zeros
173900050112     c                   z-add     o33nrf        NUM_SPED
174000050112     C                   endif
174100050112     c                   movel     svkpjbu       kpjbu
174200050112      ****
174300050112     c                   ELSE
174400050112      *   se si vuole mantenere il numero spedizione
174500050112      ****
174600050112     C                   MOVE      UYEAR         KAAA
174700050112     C                   Z-ADD     3             KCNU
174800050112     C                   Z-ADD     VABLNP        KFIL
174900050112     C     KNUF          CHAIN     FLNUF                              9999
175000050112     C                   IF        *IN99 = *OFF
175100050112     C                   ADD       1             NUFNUM
175200050112     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE
175300050112     C                   UPDATE    FLNUF
175400060612     c                   END
175500060411      *
175600050112     c                   EndIF
175700050112      **
175800050112      **              **------------------**
175900050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
176000050112      **              **------------------**
176100971215      *
176200050112     C                   MOVE      UYEAR         KAAA
176300991206     c                   IF        KAAA > 90
176400991206     C     1900          add       kaaa          VABAAS
176500991206     C                   ELSE                                                   data sped.
176600991206     C     2000          add       kaaa          VABAAS
176700991206     c                   ENDIF                                                  data sped.
176800971216      *
176900991206     C                   ENDSR
177000060620      *----------------------------------------------------*
177100060620      ** CALCOLA CHECK DIGIT -----------------------------
177200060620      *----------------------------------------------------*
177300060620     C     M1031         BEGSR
177400060620      *
177500060620     C                   MOVE      *ZERO         §TT2              3 0
177600060620     C                   CLEAR                   W0030             3 0
177700060620     C                   CLEAR                   W0010             1 0
177800060620      *
177900060620      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
178000060620     C                   DO        11            YY                2 0
178100060620      *
178200060620     C     YY            DIV       2             §CTR              3 0
178300060620     C                   MVR                     §RST              3 030
178400060620      *
178500060620     C     *IN30         IFEQ      '1'
178600060620     C     §11(YY)       MULT      3             §CTR
178700060620     C                   ELSE
178800060620     C                   Z-ADD     §11(YY)       §CTR
178900060620     C                   ENDIF
179000060620      *
179100060620     C                   ADD       §CTR          W0030
179200060620     C                   ENDDO
179300060620      *
179400060620     C     W0030         ADD       9             §TT2
179500060620     C                   MOVE      0             §TT2
179600060620     C     §TT2          SUB       W0030         DPDCHD
179700060620      *
179800060620     C                   ENDSR
179900051205      *----------------------------------------------------*
180000051205      *   DEFINIZIONE CHIAVI                               *
180100051205      *----------------------------------------------------*
180200051205     C     *INZSR        BEGSR
180300051205      *
180400991129     c     *ENTRY        PLIST
180500060613     C                   parm                    esito             1
180600110930     C                   eval      esito ='0'
180700991129      *
180800991206     C     KNUF          KLIST
180900991206     C                   KFLD                    KAAA
181000991206     C                   KFLD                    KCNU
181100991206     C                   KFLD                    KFIL
181200971215      *
181300050112     C     KTABel        KLIST
181400050112     C                   KFLD                    TBLKUT
181500050112     C                   KFLD                    TBLCOD
181600050112     C                   KFLD                    TBLKEY
181700100920      *
181800100920     C     KORM          KLIST
181900100920     C                   KFLD                    ORMPOE
182000100920     C                   KFLD                    ORMNSR
182100100920     C                   KFLD                    ORMNOR
182200100920     C                   KFLD                    ORMNRV
182300141103      *
182400141103     C     KBLP          KLIST
182500141103     C                   KFLD                    ar4AAS
182600141103     C                   KFLD                    ar4LNP
182700141103     C                   KFLD                    ar4NRS
182800141103     C                   KFLD                    ar4NSP
182900110930      *
183000110930     C     Kar4I         KLIST
183100110930     C                   KFLD                    AR4TRC
183200110930     C                   KFLD                    AR4N30
183300100920      *
183400110930     C     Kvat4E        KLIST
183500110930     C                   KFLD                    vatTRC
183600110930     C                   KFLD                    vatNOT
183700150604      *
183800150604     C     key_VTG       KLIST
183900150604     C                   KFLD                    cldCOD
184000150604     C                   KFLD                    cldDEP
184100160120      *
184200160120     C     KSosp_EDIVA   KLIST
184300160120     C                   KFLD                    Sosp_AAS
184400160120     C                   KFLD                    Sosp_LNP
184500160120     C                   KFLD                    Sosp_NRS
184600160120     C                   KFLD                    Sosp_NSP
184700160301      **
184800160301     c     KeyTBE        klist
184900160301     c                   kfld                    tbecod
185000160301     c                   kfld                    tbeke1
185100110930      *
185200050414      *------------------
185300020919      * Apro file transito o FiVAB pilotato dalla tabella
185400991124      * Caricamento Tabella PESI DPD
185500060620     c                   clear                   XX
185600991124     c                   clear                   DPP
185700991124     c     'DPP'         chain     tntbe000                           31
185800991124      *
185900991124     c                   DOW       *in31 = *off
186000991124      * Se il S.I. è indicato deve essere uguale al mio
186100991124     c                   IF        TBEatb = *BLANKS  and
186200991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
186300991124     c                   add       1             xx
186400991124     c                   movel     TBEke1        DPPK(XX)
186500000406     c                   movel     TBEuni        DDPP
186600000410     c                   movel     §DPPpkb       DPP(XX)
186700141210     c                   movel     §DPPvlb       vlb(XX)
186800991124     c                   endif
186900991124     c     'DPP'         reade     tntbe000                               31
187000991124     c                   enddo
187100971216      *
187200000908      * Caricamento Tabella Depot DPD
187300000908     c                   clear                   xx
187400000908     c                   clear                   DepK
187500091030     c                   clear                   DepK2
187600000908     c                   clear                   DepL
187700000908     c                   clear                   DepC
187800000908     c     'DEP'         chain     tntbe000                           31
187900000908      *
188000000908     c                   DOW       *in31 = *off
188100000908      * Se il S.I. è indicato deve essere uguale al mio
188200000908     c                   IF        TBEatb = *BLANKS  and
188300000908     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
188400091106     C******                         and TBEke2='I              '
188500000908     c                   add       1             xx
188600000908     c                   movel     TBEke1        DepK(XX)
188700091106     C******
188800091106      ****  chiave completa  composta dalle 2 chiavi
188900091106     c                   movel     TBEke1        DepK2(XX)
189000091106     c                   move      TBEke2        DepK2(XX)
189100091106      ****
189200000908     c                   movel     TBEuni        DDEP
189300000908     c                   movel     §DEPlnp       DepL(XX)
189400000908     c                   movel     §DEPksc       DepC(XX)
189500051219     c                   movel     §DEPtar       DepT(XX)
189600150604     c                   movel     §DEPtarVTG    DepTvtg(XX)
189700051219     c                   movel     §DEPctm       DepM(XX)
189800051219     c                   movel     §DEPwrk       DepW(XX)
189900000908     c                   endif
190000000908     c     'DEP'         reade     tntbe000                               31
190100000908     c                   enddo
190200000908      *
190300971216      * Recupero data e ora
190400971216     C                   TIME                    WORA
190500991124     C                   TIME                    W0140
190600991124      * UDATE IN GGMMAAAA
190700991124     C                   MOVE      W0140         WDTGIO
190800991124      * UDATE IN AAAAMMGG
190900991124     C     *eur          MOVEL     WDTGIO        DATA_eur
191000991124     C     *iso          MOVEL     DATA_eur      dateu
191100141103     C                   MOVEL     DATA_eur      Data_limit_BLP
191200141103      *-------------
191300141103     C***
191400141103     C* CARICO TABELLA DPD per controlli in immissione bolle Partenza
191500141103     C                   clear                   DS3IDP
191600141103     C                   Z-ADD     1             TBLKUT
191700141103     C                   MOVEL     '3I'          TBLCOD
191800141103     C                   MOVEL(P)  'DPD'         TBLKEY
191900141103     C     KTABel        CHAIN     TABEL00F                           30
192000141103     C                   IF        %Found(TABEL00F) and tblflg = *blank
192100141103     C                   MOVEL     TBLUNI        DS3IDP
192200141103     c                   subdur    §3iGGU:*d     Data_limit_BLP
192300141103     c                   movel     Data_limit_BLPDATEUMENOGG
192400141103     C                   END
192500141103     C***
192600141103      *-------------
192700991124     C                   ENDSR
192800060621      * ?------------------------------------------------------------------ */
192900060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
193000060621      * ?------------------------------------------------------------------ */
193100060621     C     *pssr         BEGSR
193200060621     C
193300060710     C                   eval      esito = '2'
193400120504     C*
193500120504     C                   eval      wrkMsg='Il File DPD ricevuto è arrivato inc'+
193600120514     C                             'omlpeto. Controllare UPLOAD ($1) '+
193700120504     C                             'IGATEAVIS con stato 2: '+
193800120504     c                                  Depot_Trs_UNO + ' : ' +
193900120504     c                                   User_Trs_UNO + ' : ' +
194000120504     c                                   %editc(Data_Trs_UNO:'X') + ' : ' +
194100120504     c                                   %editc(Time_Trs_UNO:'X') + ' : ' +
194200120504     c                                   %editc(Num_Trs_UNO:'Z')
194300090605      **    avviso tramite MAIL
194400090605     C                   exSR      eMail_alert
194500090605      **
194600060621     C
194700060621     C                   ENDSR     '*CANCL'
194800060621     C
194900090605      * ?------------------------------------------------------------------ */
195000090605      *?  Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
195100090605      * ?------------------------------------------------------------------ */
195200090605     C     eMail_alert   BEGSR
195300090605     C*
195400090605     C                   eval      wrkEml =
195500090605     C                             'Andrea.Bertocchi@bartolini.it'
195600140828     C*
195700140828     C                   eval      wrkMsg =
195800140828     C                             Identifica_file
195900140828     c                             +':/N'+
196000140828     C                             Identifica_record
196100090605     C*
196200090605     C                   eval      wrkOgg='UPLOAD DPD IGATEAVIS NON completo -
196300090605     C                             in Stato 2.'
196400090605     C*
196500140828     C*******            call(e)   'TIS701C'
196600140828     C                   call(e)   'TRTCT00R2'
196700090605     C                   parm                    wrkEml          253
196800090605     C                   parm                    wrkOgg           44
196900090605     C                   parm                    wrkMsg         5000
197000090605      *
197100090605     C                   eval      wrkEml =
197200090605     C                             'CedAlert@bartolini.it'
197300090605     C*
197400140828     C*******            call(e)   'TIS701C'
197500140828     C                   call(e)   'TRTCT00R2'
197600090605     C                   parm                    wrkEml          253
197700090605     C                   parm                    wrkOgg           44
197800090605     C                   parm                    wrkMsg         5000
197900090605      *
198000090605     C                   ENDSR
198100060614      *-------------------------------------------------------------------------
