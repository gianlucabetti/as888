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
001900171009     FazORG01L  IF   E           K DISK
002000171009      *
002100171009     FdpCDP12i  IF   E           K DISK
002200971216      *
002300150604     FTNCLD00F  IF   E           K DISK
002400061016     Ffidpo06l  IF   E           K DISK
002500141103     Ffnblp01l  IF   E           K DISK
002600171020      *
002700171020     F****** fiar403l  IF   E           K DISK
002800171020     Ffipnd03l  IF   E           K DISK
002900171020      *
003000110930     FEDivat4L  iF   E           K DISK    rename(EDIVAT00:EDIVAT4)
003100110930     FFiVAT02L  if   E           K DISK    rename(FIVAT000:FIVAT02)
003200061016      *
003300050414     FFiVAB01L  UF A E           K DISK
003400050414     FEDivab3L  UF A E           K DISK
003500160120     FEDivab2L  UF   E           K DISK    rename(EDIVAB00:EDIVAB2)
003600041215      *
003700050414     FEDiVAT3L  Uf A E           K DISK
003800050414     FFiVAT01L  Uf A E           K DISK
003900940321      *----------------------------------------------------*
004000991124      * Schiere per conversione peso DPD
004100991124     D DPPK            S             15    DIM(30)                              CODICE NAZIONE
004200991124     D DPP             S              7S 1 DIM(30)                              CODICE NAZIONE
004300141210     D VLB             S              5S 3 DIM(30)                              CODICE NAZIONE
004400971210      *-------------------
004500000907      * schiere dei Depositi DPD dai quali rivavare la linea
004600051219     D DEPK            S             15    DIM(300)                             COD DEP
004700091106     D DEPK2           S             30    DIM(300)                             COD DEP + HUB
004800051219     D DepL            S              3S 0 DIM(300)                             COD Linea
004900051219     D DepC            S              7S 0 DIM(300)                             COD Cliente
005000051219     D DepT            S              3s 0 DIM(300)                             COD tariffa
005100150604     D DepTvtg         S              3s 0 DIM(300)                             COD tariffa VTG
005200051219     D DepM            S              2A   DIM(300)                             COD trat.merce
005300051219     D DepW            S              1A   DIM(300)                             EDIVAB --> 'S'
005400171009     D*------------------------------------------------------------------------*
005500171009     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
005600000907      *-------------------
005700060612     D Lung_Parcel     S              3  0
005800060614     D Tipo_error      S              1  0
005900060612      *-------------------
006000991129     D W015A           S             15
006100091109     D W030A           S             30
006200000223     D W0140           S             14  0
006300991129     D WORA            S              6  0
006400991129     D XX              S              3  0 INZ
006500991129     D WDTGIO          S              8  0
006600991129     D DATEU           S              8  0
006700141103     D DATEUMENOGG     S              8  0
006800991129     D DATA_eur        S               D   DATFMT(*eur)
006900141103     D DATA_limit_BLP  S               D   DATFMT(*iso)
007000991206     D KAAA            S                   LIKE(NUFAAA)
007100991206     D KCNU            S                   LIKE(NUFCNU)
007200991206     D KFIL            S                   LIKE(NUFFIL)
007300050112     D NUM_Sped        s                   LIKE(vabnsp)
007400160120     d  Sosp_AAS       s                   like(VATAAS)
007500160120     d  Sosp_LNP       s                   like(VATLNP)
007600160120     d  Sosp_NRS       s                   like(VATNRS)
007700160120     d  Sosp_NSP       s                   like(VATNSP)
007800050112     D svkpjbu         s                   like(kpjbu)
007900060413      *-------------------
008000991129     D KPJBA         E DS
008100000406     D DDPP          E DS
008200000907     D DDEP          E DS
008300130618     D OG143         E DS
008400141103     D DS3IDP        E DS
008500160301     D Dlpd          E DS
008600060608      **?------------------------------------------------------------------ */
008700060608     C*? Ds Scrittura del VAT "E"  passaggio nuovi campi BIC3
008800060608      **?------------------------------------------------------------------ */
008900060612     D XTOEBCDDS     E DS
009000060612     D DVate         E DS
009100060612     D TIDP100       E DS                  EXTNAME(TIDP100W)
009200060620      *
009300060620     D DoveVado        DS
009400060620     d  Italia39                      2
009500060620     d  CapItalia                     5
009600060620     d  TipoPacco                     2
009700060620     d  ChkDigit                      1
009800060620      *
009900060620     D DsParcel        DS
010000060620     d  Parcel0                       1
010100060620     d  Depot3                        3
010200060620     d  NoveNove                      2
010300060620     d  NumParcel                     8
010400060620      *
010500060710     D Parcel14        S             14
010600150604     D Depot04         S              4
010700150604      *
010800060911     D DepotParcel     S              3
010900060620      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
011000060620     D                 DS
011100060620     D  DPDLNP                 1      3  0
011200060620     D  DPDGMS                 4      7  0
011300060620     D  DPDNDS                 8     11  0
011400060620     D  §11                    1     11  0
011500060620     D                                     DIM(11)
011600060620     D  DPDCHD                12     12  0
011700060620     D  DPDBRC                 1     12  0
011800060608      **?------------------------------------------------------------------ */
011900050112      * Numeratore Bolle (302)
012000050112     D trul33ds      E DS
012100050112     D Ds3C          E DS
012200070202     D TISI95DS      E DS
012300060612      * ?================================================================== */
012400060612      * ?   * Campi da tradurre con interi
012500060612     D atoi            PR            10I 0
012600060612     D                                     EXTPROC('atoi')
012700060612     D  Char                           *
012800060612     D                                     VALUE
012900060612     D                                     OPTIONS(*STRING)
013000060612      * ?   * Campi da tradurre con decimali
013100060612     D atof            PR             8F
013200060612     D                                     EXTPROC('atof')
013300060612     D  Char                           *
013400060612     D                                     VALUE
013500060612     D                                     OPTIONS(*STRING)
013600060612      * ?   *--------------------------------------------------------------*
013700060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
013800060612      * ?   *--------------------------------------------------------------*
013900060612      *
014000060612      * ?   * Campi x decodifica * (INPUT  del Record)
014100060612     D  Dati           s           1500
014200060612     D Separa_campi    s              1
014300060612     D Decimal_Separ   s              1
014400060612      * ?   * Campi decodificati * (OUTPUT del Record)
014500060612      *       Schiere di Output / Campi di Output
014600060614     D  Alfa40         s             40
014700060612     d  Sk             s              3u 0
014800060612     D    NR_campi     s              3u 0
014900060612     D  Campi_Record   ds
015000060612     D    Dato_sk                    40    DIM(100)
015100060612     D  Campi_Lunghi   ds
015200060612     D    LunDato_sk                  3u 0 DIM(100)
015300060612     D Campi_Numerici  ds
015400060612     D    DatoNum_sk                  1    DIM(100)
015500060612     D Campi_Decimali  ds
015600060612     D    Decimal_sk                  3u 0 DIM(100)
015700060612      * ?   *--------------------------------------------------------------*
015800060612     D  position       s              3  0 INZ(0)
015900060612
016000060612     D  se_errore      s              1    inz(' ')
016100060612     D  Msg_Err        s            132    inz(' ')
016200060614     D  Msg_Vin_80     s             80    inz(' ')
016300060612      * ?* ------------------------------------------------------ *
016400060612      * Records di controllo mandati nell'IGATEAVIS
016500060612      * ?   * Dati Record UNO *
016600060612     DDepot_Trs_UNO    s             10
016700060612     D User_Trs_UNO    s             10
016800060612     D Data_Trs_UNO    s              8  0
016900060612     D Time_Trs_UNO    s              6  0
017000060612     D  Num_Trs_UNO    s              9  0
017100060612      * ?   * Dati Record DUE *
017200060612     D Testa_Sk        s             10    DIM(50)
017300060612      * ?   * Dati Record TRE *
017400060612     D  Num_Trs_FIN    s              9  0
017500120504     d non_inviato_prima...
017600120504     d                 s              1
017700060710      **
017800140827     d non_completamente_tradotto...
017900140827     d                 s              1    inz(' ')
018000140828     d Identifica_file...
018100140828     d                 s            100    inz(' ')
018200140828     d Identifica_record...
018300140828     d                 s            250    inz(' ')
018400140827      **
018500060710     D Digits          C                   '0123456789'
018600130819     d Normal_Parcel   c                   101
018700130819     d Small_Parcel    c                   136
018800130819     d Return_Sender   c                   298
018900130819     d Return          c                   300
019000140409     d Gomme_365       c                   365
019100140409     d Gomme_366       c                   366
019200140409     d Gomme_367       c                   367
019300111110     d Normal_Parcel_327...
019400130819     d                 c                   327
019500111110     d Normal_Parcel_329...
019600130819     d                 c                   329
019700111110     d Small_Parcel_328...
019800130819     d                 c                   328
019900160120     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
020000111006      *
020100060612      * ?================================================================== */
020200060612      *   Ciclo principale
020300060413      * ?********************************************************************/
020400060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
020500060612     C*
020600060612     C                   if        not %open(tivin00r)
020700060612     C                   open      tivin00r
020800060612     C                   endif
020900060612      **
021000060612      * ?------------------------------------------------------------------ */
021100060612     C*? Controllo dati arrivati da DPD
021200060612      * ?------------------------------------------------------------------ */
021300060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
021400060612      * ?- controllando che la trasmissione sia completa.
021500060612
021600060612      * ?              /*---------------------- */
021700060612     c                   exsr      check_Trasm
021800060612      * ?              /*---------------------- */
021900060612
022000060612      **
022100060613      **  Errore di trasmissione x tutti i records
022200060613      **   --> file in errore
022300060613     c                   if        se_errore = 'S'
022400060612
022500060612
022600060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
022700060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
022800060613     C                               >> Farsi rimandare i DATI !!'
022900060612
023000060612      * ?  Scrive su tutti i records il tipo di errore
023100060612     c     *start        setll     tivin00r
023200060612     c                   read      tivin00r
023300060612     c                   dow       not %eof(tivin00r)
023400060612     C                   eval      vinMSG = Msg_Err
023500060612     C                   eval      vinFLG = '2'
023600060613     c                   eval      esito  = '2'
023700060612     c                   update    tivin000
023800060612     c                   read      tivin00r
023900090605     c                   endDO
024000090605      **
024100090605      **    avviso tramite MAIL
024200090605     C                   exSR      eMail_alert
024300060612      **
024400060612     c                   else
024500060614      **
024600060612      * ?------------------------------------------------------------------ */
024700060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
024800060612      * ?------------------------------------------------------------------ */
024900060612      * ?              /*---------------------- */
025000060612     c                   exsr      Importa_Msg
025100060612      * ?              /*---------------------- */
025200060612
025300060614     c                   end
025400060614
025500060612     C                   if        %open(tivin00r)
025600060612     C                   close     tivin00r
025700060612     C                   endif
025800060612      *
025900060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
026000060614      *
026100110930     c*********          if        almeno_uno ='S' and esito ='1'
026200110930     C*********          eval      esito ='0'
026300110930     C*********          endif
026400060614      *
026500140827     C                   if        non_completamente_tradotto = 'S'
026600140827     c                   exsr      email_alert
026700140827     c                   end
026800140827      *
026900060612     c                   SETON                                        LR
027000060612      * ?================================================================== */
027100060612      *? Controlla la trasmissione   se completa
027200060612      * ?------------------------------------------------------------------ */
027300060612     c     Check_Trasm   Begsr
027400060612
027500060612     C                   clear                   se_errore
027600080917     C                   clear                   Riga_iNIZIo       1
027700080917     C                   clear                   Riga_fINe         1
027800060612      ** primo  record
027900060612     c     *start        setll     tivin00r
028000071025     c     rileggi       tag
028100060612     c                   read      tivin00r
028200060612     c                   if        not %eof(tivin00r)
028300071025      *
028400071025     c                   movel(p)  VINDTA        dati
028500071025     c                   if        dati = *blank
028600071025      * le prime righe potrebbero essere vuote prima di iniziare il messaggio
028700071025      * le leggo e le escludo.
028800071025     C                   eval      vinFLG = '1'
028900071025     c                   update    Tivin000
029000071025     c                   goto      rileggi
029100071025     c                   end
029200060612      * ?              /*---------------------- */
029300060612     c                   exsr      Decod_Record
029400060612      * ?              /*---------------------- */
029500060612     c                   endif
029600060612      **
029700060612      ** ultimo record
029800060612     c     *hival        setll     tivin00r
029900080917     c     leggiAncora   tag
030000060612     c                   readp     tivin00r
030100060612     c                   if        not %eof(tivin00r)
030200080917      *
030300080917     c                   movel(p)  VINDTA        dati
030400080917     c                   if        dati = *blank
030500080917      * le ultime righe potrebbero essere vuote prima di finire il messaggio
030600080917      * le leggo e le escludo.
030700080917     C                   eval      vinFLG = '1'
030800080917     c                   update    Tivin000
030900080917     c                   goto      leggiAncora
031000080917     c                   end
031100060612      * ?              /*---------------------- */
031200060612     c                   exsr      Decod_Record
031300060612      * ?              /*---------------------- */
031400060612     c                   endif
031500060612
031600060612      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
031700060612      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
031800060612      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
031900080917     C******             if        num_trs_UNO <> num_trs_FIN
032000080917     C****** Non è + vero poichè DPD invia trasmissioni di + trasmissioni quindi
032100080917     C****** Non si può confrontare il numero trsm.
032200080917     C                   if        Riga_Inizio = *blank or Riga_Fine = *blank
032300060612      * ?-----> Errore
032400060612     C                   eval      se_errore = 'S'
032500060612
032600060612     c                   end
032700060612
032800060612     c                   endSR
032900060612      * ?------------------------------------------------------------------ */
033000060612      *? Importa i records della tramsissione
033100060612      * ?------------------------------------------------------------------ */
033200060612     c     Importa_Msg   Begsr
033300060612
033400060614      * per controllare se almeno un record è stato importato sul VAB
033500060614     c                   clear                   Almeno_Uno        1
033600060614
033700060612     c     *start        setll     Tivin00r
033800060612     c                   read      Tivin00r
033900060612
034000060612     c                   dow       not %eof(Tivin00r)
034100060614
034200060614      * solo i record sflaggati da rielaborare
034300060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
034400060612
034500060612      ** Controlli formali sui campi
034600060612     c                   clear                   se_errore
034700060620     C                   clear                   Msg_Vin_80
034800060612
034900060612      ** Decodifica record a campi variabili
035000060612      * ?              /*---------------------- */
035100060612     c                   exsr      Decod_Record
035200060612      * ?              /*---------------------- */
035300060612
035400060612      **  Se presente un errore nel record emette una segnalazione msg
035500060612     c                   if        se_errore ='S'
035600060614     C                   eval      vinMSG = Msg_Vin_80
035700060621     c                   end
035800140827      *
035900140827      * Dopo il controllo Formale di RIGA occorre quindi filtrare quando
036000140827      *  DEVE saltare alla riga successiva ricevuta e NON FARE nulla.
036100140827      *  ma deve dare solo l'errore.
036200140827     c                   If        salta_record = *blank
036300110929      *
036400110929      * ?              /*---------------------- */
036500110929      *  Controlla se il Parcel non fosse già presente su EDIVAT o FIVAT
036600110929      * ?              /*---------------------- */
036700110930     c                   clear                   destino          35
036800110929      *
036900110930      *  SE GIA' PRESENTE NON VIENE CARICATO
037000110930     c                   if        %subst(dati:1:1) <> '#'
037100110930      *
037200110930      *  Controlla se la spedizione è già arrivata precedentemente
037300110930      *    sulle bolle in partenza o da confermare
037400160120      *  Oppure se è in attesa poichè arrivata da GEODATA senza ancora lo SCAN 05
037500160120      *  ossia in ATTESA dello SCAN 05.
037600110930     c                   clear                   GIA_PRESENTE      1
037700160120     c                   clear                   IN_ATTESA         1
037800111006      *
037900111006      **  se non è un reso  controlla se già arrivato
038000111006     c                   if        I1SERVICE <>  300
038100110930     c                   EXSR      check_PARCEL
038200111006     c                   end
038300160120      * -----
038400110930      *  se fosse già presente
038500110930     c                   IF           GIA_PRESENTE = 'S'
038600110929     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(i1PARCEL) +
038700110929     c                                          ' già ricevuto precedentemente!'
038800110929     C                   eval      vinMSG = Msg_Vin_80
038900160120      * -----
039000160120     c                   elseIF       IN_ATTESA = 'S'
039100160120      * ?              /*---------------------- */
039200160120     c                   exsr      UPD_DA_GEO
039300160120      * ?              /*---------------------- */
039400160120     C                   eval      Msg_Vin_80 = 'Parcel :' + %trim(i1PARCEL) +
039500160120     c                                          ' in Attesa da GEO! Ora in elab-
039600160208     c                             orazione'
039700160120     C                   eval      vinMSG = Msg_Vin_80
039800160120      * -----
039900110929     c                   else
040000160120      * -----
040100060612      *  con il record in canna scrive direttamente il VAB ed il VAT
040200060612      * ?              /*---------------------- */
040300060612     c                   exsr      WRT_VAB_VAT
040400060612      * ?              /*---------------------- */
040500060614      *  Problemi nella decodifica dei campi VAB/VAT
040600060614     c                   if        se_errore ='S'
040700060614     C                   eval      vinMSG = Msg_Vin_80
040800060614     c                   end
040900111110
041000110929     c                   endIF
041100160120
041200110930     c                   endIF
041300060612
041400060621      *  Sempre Record OK
041500140827     c                   endIF
041600140827
041700060621     C                   eval      vinFLG = '1'
041800060621
041900060621      *  x errore bloccante nella composizione del VAB/VAT
042000060621     c                   if        err_bloccante ='S'
042100060621     C                   eval      vinFLG = '2'
042200060710     c                   eval      esito  = '1'
042300060621     c                   end
042400060612
042500060612     c                   update    Tivin000
042600060614     c                   endIF
042700060612
042800060612     c                   read      Tivin00r
042900060612     C                   ENDdo
043000060612      **
043100060612     c                   endSR
043200110930      * ?------------------------------------------------------------------ */
043300110930      *?    CONTROLLA SE il parcel è arrivato in una precedente trasmissione
043400110930      * ?------------------------------------------------------------------ */
043500110930     c     check_PARCEL  Begsr
043600130619      **
043700130619      ** controlla la Linea che deve essere DPD
043800130619     c                   z-add     0             LineaP            3 0
043900110930      **
044000110930      **  se non ancora confermato CMR
044100110930     C                   eval        vatTRC = 'E'
044200110930     C                   eval        vatNOT = i1PARCEL
044300110930     C     Kvat4E        setll     edivat4l
044400110930     C     vatTRC        reade     edivat4l
044500110930     c                   IF        not %EoF(edivat4l) AND
044600110930     C                             I1PARCEL = %SUBST(vatNOT:1:14)
044700160120      *--
044800160120      *-   Se era in Sospeso perchè scritto dalla parte del GEODATA
044900160120      *-   in attesa dello SCAN 05 di (Partito dal DEPOT di PARTENZA quindi in viaggio)
045000160120      *--
045100160120     c                   if        vatCMR = GEO_in_Sospeso
045200160120     c                   z-add     vatAAS        Sosp_AAS
045300160120     c                   z-add     vatLNP        Sosp_LNP
045400160120     c                   z-add     vatNRS        Sosp_NRS
045500160120     c                   z-add     vatNSP        Sosp_NSP
045600160120     c                   eval      IN_ATTESA  = 'S'
045700160120     c                   leaveSR
045800160120     c                   end
045900160120      *--
046000130619     c                   z-add     vatLNP        LineaP
046100110930     c                   end
046200110930      **
046300110930      **  se non ancora confermato da Cliente
046400130619     c                   if        LineaP = 0
046500110930     C                   eval        vatTRC = 'E'
046600110930     C                   eval        vatNOT = i1PARCEL
046700110930     C     Kvat4E        setll     fivat02l
046800110930     C     vatTRC        reade     fivat02l
046900110930     c                   IF        not %EoF(fivat02l) AND
047000110930     C                             I1PARCEL = %SUBST(vatNOT:1:14)
047100130619     c                   z-add     vatLNP        LineaP
047200110930     c                   end
047300130619     c                   end
047400160120      **
047500110930      **  se già confermato
047600130619     c                   if        LineaP = 0
047700171020      **
047800171020     C****               eval        AR4TRC = 'I'
047900171020     C****               eval        AR4N30 = i1PARCEL
048000171020     C**** Kar4I         setll     fiar403l
048100171020     C**** ar4TRC        reade     fiar403l
048200171020     c****               IF        not %EoF(fiar403l) AND
048300171020     C****                         I1PARCEL = %SUBST(AR4N30:1:14)
048400171020      **
048500171020      **  Controlla con il Parcel
048600171020     C     i1PARCEL      chain     fipnd03l
048700171020     c                   IF        %Found(fipnd03l)
048800171020      **
048900141103      ***
049000141103      *  controlla se su BLP sono passati più di tot.giorni (tab.3I) oppure
049100141103      *  se deve essere invece considerato il parcel veramente doppio.
049200141103     c     kblp          chain     fnblp01l
049300141103     c                   if        %Found(fnblp01l)
049400141103     C                   Z-ADD     BLPMGS        Data_BLP          8 0
049500141103     C                   MOVEL     BLPAAS        Data_BLP
049600141103    5C     Data_BLP      IFGE      DateuMENOgg
049700141103     c     blpcca        andne     '7'
049800141103     c                   z-add     blpLNP        LineaP
049900141103     c                   end
050000141103     c                   end
050100141103      ***
050200111003     c                   end
050300130619     c                   end
050400130619      *
050500130619      *   controlla che sia DPD con il Network e non con la linea
050600130619     c                   if        LineaP > 0
050700130619     c     LineaP        chain     azorg01l
050800130619     c                   if        %Found(azorg01l)
050900130619     c                   eval      OG143 = orgDE3
051000130619     c                   if         §OGNTW = 'DPD'
051100130619     c                   eval      GIA_PRESENTE = 'S'
051200130619     c                   end
051300130619     c                   endif
051400130619     c                   end
051500130619      **
051600110930     c                   endSR
051700060612      * ?------------------------------------------------------------------ */
051800060612      *?    Decodifica record a campi variabili
051900060612      * ?------------------------------------------------------------------ */
052000060612     c     Decod_Record  Begsr
052100060612
052200060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
052300060612     c                   movel(p)  VINDTA        dati
052400151215     c     riprova       tag
052500110930     c                   EVAL      err_bloccante =' '
052600120305      **
052700120305      * ?              /*---------------------- */
052800141103     c                   exsr      se_Amp_chr
052900120305      * ?              /*---------------------- */
053000141103      **
053100141103      * ?              /*---------------------- */
053200141103     c                   exsr      se_doppioApice
053300141103      * ?              /*---------------------- */
053400060612      **
053500060612      * ?              /*---------------------- */
053600060612     c                   exsr      Split_Rec
053700060612      * ?              /*---------------------- */
053800060612      **
053900060612      **  Records di Intestazione e di fine Msg
054000060612     c                   if        %subst(dati:1:1) ='#'
054100060612      * ?              /*---------------------- */
054200060612     c                   exsr      Rec_Speciali
054300060612      * ?              /*---------------------- */
054400060612     c                   Else
054500140827      **
054600140827      * spostata qua la pulizia del msg di errore da riportare sul vinMSG
054700140827     c                   clear                   Msg_Err
054800140827      ** controlla che lo split del record sia corretto testando la presenza del
054900140828      ** nome BARTOLINI sul campo (20) della schiera.
055000140827      * ?              /*---------------------- */
055100140828     c                   exsr      xDISALLineata
055200140827      * ?              /*---------------------- */
055300060612      ** conta le righe
055400140827     c                   If        salta_record = *blank
055500151215     c                             and RITRADURRE = *blank
055600060612      * ?              /*---------------------- */
055700060612     c                   exsr      Rec_Dettaglio
055800060612      * ?              /*---------------------- */
055900140827     c                   Endif
056000060612     c                   End
056100060612      **
056200151215     c                   if        RITRADURRE = 'S'
056300151215     c                   goto      riprova
056400151215     c                   Endif
056500151215      **
056600060612     c                   Endsr
056700060612      * ?------------------------------------------------------------------ */
056800060612      *?      Record di Testata e di Fine Msg
056900060612      * ?------------------------------------------------------------------ */
057000060612     c     Rec_Speciali  Begsr
057100060612      **
057200060612      * ?  Prima riga
057300060612     c     'FILE'        SCAN      dati:2        position
057400060612     c                   if        %Found and position < 5
057500140828     C                   eval      Identifica_file = dati
057600060612     c                   exsr      Rec_UNO
057700080917     C                   move      'S'           Riga_iNIZIo
057800060612     c                   ElSe
057900060612     c                   End
058000060612      **
058100060612      * ?  Seconda riga (testate dei campi)
058200060612     c     'DEF'         SCAN      dati:2        position
058300060612     c                   if        %Found and position < 5
058400060612     c                   exsr      Rec_DUE
058500060612     c                   End
058600060612      **
058700060612      * ?  Ultima riga
058800060612     c     'END'         SCAN      dati:2        position
058900060612     c                   if        %Found and position < 5
059000060612     c                   exsr      Rec_LAST
059100080917     C                   move      'S'           Riga_fINe
059200060612     c                   End
059300060612      **
059400060612     c                   Endsr
059500060612      * ?------------------------------------------------------------------ */
059600060612      *?      Primo Record
059700060612      * ?------------------------------------------------------------------ */
059800060612     c     Rec_UNO       Begsr
059900060612      **
060000060612      ** esempio di record :
060100060612      * ?   #FILE;depot0016;N00;20050916;093045;4711;
060200060612      **
060300060612      **  Inizializza i campi della trasmissione
060400060612     C                   eval      Depot_Trs_UNO = *blank
060500060612     C                   eval      User_Trs_UNO  = *blank
060600060612     C                   eval      Data_Trs_UNO  = 0
060700060612     C                   eval      Time_Trs_UNO  = 0
060800060612     C                   eval      Num_Trs_UNO   = 0
060900060612
061000060612      **   Memorizza il numero della trasm. da confrontare con l'ultimo record.
061100060612      **     Tutto il dettaglio deve essere compreso entro questi 2 records
061200060612      **    altrimenti il msg.non è completo e bisogna farsi rimandare il file.
061300060612
061400060612      * ? Nel 2° campo di testata c'è il Dispatch Depot
061500060612     C                   z-add     2             sk
061600060612     c                   eval      Depot_Trs_UNO= %subst(Dato_sk(sk):1:+
061700060612     c                             LunDato_sk(sk))
061800060612
061900060612      * ? Nel 3° campo di testata c'è lo USER in Delis
062000060612     C                   z-add     3             sk
062100060612     c                   eval      User_Trs_UNO  = %subst(Dato_sk(sk):1:+
062200060612     c                             LunDato_sk(sk))
062300060612
062400060612      * ? Nel 4° campo di testata c'è la data trasmissione
062500060612     C                   z-add     4             sk
062600060612     c                   if        DatoNum_sk(sk) = 'S'
062700060612     c                   eval      Data_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
062800060612     c                   End
062900060612
063000060612      * ? Nel 5° campo di testata c'è il time di trasmissione
063100060612     C                   z-add     5             sk
063200060612     c                   if        DatoNum_sk(sk) = 'S'
063300060612     c                   eval      Time_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
063400060612     c                   End
063500060612
063600060612      * ? Nel 6° campo di testata c'è il nr.seriale di trasmissione
063700060612     C                   z-add     6             sk
063800060612     c                   if        DatoNum_sk(sk) = 'S'
063900060612     c                   eval      num_trs_UNO = atoi(%TRIM(Dato_sk(sk)))
064000060612     c                   End
064100060612      **
064200060612     c                   Endsr
064300060612      * ?------------------------------------------------------------------ */
064400060612      *?      Secondo Record
064500060612      * ?------------------------------------------------------------------ */
064600060612     c     Rec_DUE       Begsr
064700060612      **
064800060612      ** esempio di record :
064900060612      * ?   #DEF;IGATEAVIS;SDEPOT;DATE;TIME;PARCELNO;SERVICE;RDEPOT;DNAME1;DNAME2;DCONT....
065000060612      **
065100060612      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
065200060612     C                   clear                   Testa_Sk
065300060612
065400060612      * ? Dal 3° campo troviamo i nomi delle intestazioni dei campi C.Request
065500060612     C     3             do        50            sk
065600060612     c                   eval      Testa_Sk(sk-2) = %subst(Dato_sk(sk):1:+
065700060612     c                             LunDato_sk(sk))
065800060612     c                   enddo
065900060612      **
066000060612     c                   Endsr
066100060612      * ?------------------------------------------------------------------ */
066200060612      *?      Ultimo  Record
066300060612      * ?------------------------------------------------------------------ */
066400060612     c     Rec_LAST      Begsr
066500060612      **
066600060612      ** esempio di record :
066700060612      * ?   #END;4711;
066800060612      **
066900060612      **  Inizializza i campi della trasmissione
067000060612     C                   eval      Num_Trs_fin = 0
067100060612      **
067200060612      **  Nell'ultimo record di chiusura deve esserci lo stesso numero di trasmissione
067300060612      **  impostato nel primo record.
067400060612      **
067500060612      * ? Nel 2° campo di fine trsm. c'è il nr.seriale di trasmissione
067600060612     C                   z-add     2             sk
067700060612     c                   if        DatoNum_sk(sk) = 'S'
067800060612     c                   eval      num_trs_fin = atoi(%TRIM(Dato_sk(sk)))
067900060612     c                   End
068000141103      **
068100141103     c                   Endsr
068200141103      * ?------------------------------------------------------------------ */
068300141103      *?    Se ricevuto &amp; va sostituito con 5 blanks
068400141103      * ?------------------------------------------------------------------ */
068500141103     c     se_amp_chr    Begsr
068600141103      **
068700141103      **   se il record contiene "&amp;"  è la traduzione di un carattere estero accentato
068800141103      **   va sostituito con 4 blanks
068900141103     c                   z-add     1             amp               3 0
069000141103     c                   z-add     0             da_amp            3 0
069100151215     c     '&amp;'       scan      dati:amp      da_amp
069200151215     c*****'&amp'        scan      dati:amp      da_amp
069300141103     c                   if        %Found
069400151215     c                   eval      %subst(dati:da_amp:5) = *blanks
069500151215     c******             eval      %subst(dati:da_amp:4) = *blanks
069600141103     c                   end
069700141103      **
069800060612     c                   Endsr
069900060612      * ?------------------------------------------------------------------ */
070000120305      *?    Problema di caratteri dell'est o nordici     racchiusi fra 2 doppi apici
070100060612      * ?------------------------------------------------------------------ */
070200120305     c     se_DoppioApiceBegsr
070300060612      **
070400160919      ** AL MOMENTO SOSTITUISCO LA VECCHIA REGOLA SOTTO modificando solo in modo che il "
070500160919      ** venga sostituito con *blank.
070600160919     c                   eval      dati = %xlate('"':' ':dati)
070700160919     c                   leavesr
070800160919      **-------------------------------------
070900160919      **
071000160919      **
071100120305      **   contengono una traduzione del carattere che finisce con (;) es.: [&amp;] x identificare
071200120305      **   il carattere (&)...... il problema che ci si presenta è il (;) che viene interpretato
071300120305      **   come separatore di campo mentre invece fa solo parte della traduzione de carattere
071400120305      **   ASCII utilizzato al nord o est Europa.
071500120305     c                   clear                   apici_2           1
071600120305     c                   eval      apici_2 = '"'
071700120305      * cerca di individuare se c'è una dicitura che è messa fra (")
071800120305      *  considera l'estremo sinistro
071900120305     c                   z-add     1             dove              3 0
072000120305      *****
072100120305     c     ritenta       tag
072200120305      *****
072300120305     c     apici_2       scan      dati:dove     da_dove           3 0
072400120305     c                   if        %Found
072500120305      *  considera l'estremo destro
072600120305     c     da_dove       add       1             dove
072700120305     c     apici_2       scan      dati:dove     a_dove            3 0
072800120305      *  se esiste anche un estremo destro controlla se in mezzo c'è un (;)
072900120305      *  da eliminare .
073000120305     c                   if        %Found
073100120305     c                   clear                   punto_virgola     1
073200120305     c                   eval      punto_virgola = ';'
073300120305     c     da_dove       add       1             dove
073400120305     c     punto_Virgola scan      dati:dove     in_mezzo          3 0
073500120305      * se il punto e virgola si trova in mezzo ai doppi apici lo deve sostituire
073600120305      * con un blank
073700120305     c                   if        in_mezzo < a_dove
073800120305     c                   eval      %subst(dati:in_mezzo:1) = ' '
073900120305     c     a_dove        add       1             dove
074000120305     c                   goto      ritenta
074100120305     c                   end
074200120305     c                   end
074300120305     c                   end
074400120305      **
074500120305     c                   Endsr
074600140827      * ?------------------------------------------------------------------ */
074700140827      *?      Controllo preventivo di traduzione corretta della riga
074800140827      * ?------------------------------------------------------------------ */
074900140828     c     xDISALLINEAta Begsr
075000140827      **
075100140827     c                   clear                   salta_record      1
075200151215     c                   clear                   RITRADURRE        1
075300151215      **
075400151215      **   prova a sistemare il record andando ad aggiungere il (;) dove
075500151215      **    probabilmente era stato tolto.
075600151215     c                   if              Dato_sk(19)  = 'BARTOLINI'
075700151215     c                   eval      %subst(dati:da_amp:1) = ';'
075800151215     c                   eval      RITRADURRE = 'S'
075900151215     c                   LEAVESR
076000151215     c                   end
076100140827      **
076200140828      **  Se il nome BARTOLINI è al posto giusto significa che i campi precedenti
076300140828      **   si presume siano nei campi correttamente separati dai (;) e che DPD
076400140828      **    abbia scritto bene il RECORD.
076500140828      **   Purtroppo NON è possibile prevedere altri tipi di errori fatti casualmente
076600140828      **    dai vari DEPOT sui records.
076700140827      **
076800140828     c                   if              Dato_sk(20) <> 'BARTOLINI'
076900140827      **
077000140828     c                   eval      Identifica_record = dati
077100140827     c                   eval      salta_record = 'S'
077200140827      * ?-----> Errore
077300140827     c                   eval      tipo_error = 5
077400140827     c                   move      'S'           err_bloccante
077500140827     c                   eval      non_completamente_tradotto = 'S'
077600140827     c                   exsr      Field_Error
077700140827      **
077800140827     c                   End
077900140827      **
078000140827     c                   Endsr
078100120305      * ?------------------------------------------------------------------ */
078200120305      *?      Riga Bolla Import da controllare e tradurre
078300120305      * ?------------------------------------------------------------------ */
078400120305     c     Rec_Dettaglio Begsr
078500120305      **
078600060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
078700060612      **
078800060612     c                   clear                   TIdp100
078900140827     c******                   clear                   Msg_Err
079000060614      **
079100060612      * ? Nel 1° campo : HUB DPD che invia i dati    (Oblligatorio)  SDEPOT
079200060612      **                                                             HUB DPD D'INVIO
079300060612     C                   z-add     1             sk
079400060612     c                   If        LunDato_sk(sk) =  0
079500060612      * ?-----> Errore
079600060614     c                   eval      tipo_error = 1
079700060614     c                   exsr      Field_Error
079800060612     c                   Else
079900060612     c                   eval      i1SDEPOT   = %subst(Dato_sk(sk):1:+
080000060612     c                                              LunDato_sk(sk))
080100060612     c                   End
080200060612      **
080300060612      * ? Nel 2° campo : Data di Trasmissione DPD    (Oblligatorio)  DATE
080400060612      **                                                             DATA TRASM.
080500060612     C                   add       1             sk
080600060612     c                   If        DatoNum_sk(sk) = 'N' or
080700060612     c                             LunDato_sk(sk) =  0
080800060612      * ?-----> Errore
080900060614     c                   eval      tipo_error = 2
081000060614     c                   exsr      Field_Error
081100060612     c                   Else
081200060612     c                   eval      i1DATE   = atoi(%TRIM(Dato_sk(sk)))
081300060612     c                   End
081400060612      **
081500060612      * ? Nel 3° campo : Orario di Trasmissione DPD                  TIME
081600060612      **                                                             ORA TRASM.
081700060612     C                   add       1             sk
081800060612     c                   If        DatoNum_sk(sk) = 'N' and
081900060612     c                             LunDato_sk(sk) >  0
082000060612      * ?-----> Errore
082100060614     c                   eval      tipo_error = 3
082200060614     c                   exsr      Field_Error
082300060612     c                   Else
082400060612     c                   eval      i1TIME   = atoi(%TRIM(Dato_sk(sk)))
082500060612     c                   End
082600060612      **
082700060612      * ? Nel 4° campo : Numero di Parcel            (Oblligatorio)  PARCELNO
082800060612      **                                                             NR.PARCEL DPD
082900060612     C                   add       1             sk
083000060612     c                   If        LunDato_sk(sk) =  0
083100060612      * ?-----> Errore
083200060614     c                   eval      tipo_error = 1
083300060614     c                   exsr      Field_Error
083400060612     c                   Else
083500060612     c                   eval      i1PARCEL = %subst(Dato_sk(sk):1:+
083600060612     c                                              LunDato_sk(sk))
083700060621      ** controlla lunghezza del Parcel
083800060621     c                   clear                   Lung_Parcel
083900060621     c                   eval      Lung_Parcel = %Len(%trim(i1PARCEL))
084000060612     c                   End
084100060612      **
084200060612      * ? Nel 5° campo : Codice di Servizio          (Oblligatorio)  SERVICE
084300060612      **                                                             SERVICE CODE
084400060612     C                   add       1             sk
084500060612     c                   If        DatoNum_sk(sk) = 'N' or
084600060612     c                             LunDato_sk(sk) =  0
084700060612      * ?-----> Errore
084800060614     c                   eval      tipo_error = 2
084900060614     c                   exsr      Field_Error
085000130819     c                   eval      i1SERVICE  = Normal_Parcel
085100060612     c                   Else
085200060612     c                   eval      i1SERVICE  = atoi(%TRIM(Dato_sk(sk)))
085300060612     c                   End
085400060612      **
085500060612      * ? Nel 6° campo : HUB Bartolini               (Oblligatorio)  RDEPOT
085600060612      **                                                             HUB Bartolini
085700060612     C                   add       1             sk
085800060612     c                   If        LunDato_sk(sk) =  0
085900060612      * ?-----> Errore
086000060614     c                   eval      tipo_error = 1
086100060614     c                   exsr      Field_Error
086200060612     c                   Else
086300060612     c                   eval      i1RDEPOT   = %subst(Dato_sk(sk):1:+
086400060612     c                                              LunDato_sk(sk))
086500060612     c                   End
086600060612      **
086700060612      * ? Nel 7° campo : Nome Destinatario           (Oblligatorio)  DNAME1
086800060612      **                                                             NOME DESTINATARIO
086900060612     C                   add       1             sk
087000060612     c                   If        LunDato_sk(sk) =  0
087100060612      * ?-----> Errore
087200060614     c                   eval      tipo_error = 1
087300060614     c                   exsr      Field_Error
087400060612     c                   Else
087500060612     c                   eval      i1DNAME1   = %subst(Dato_sk(sk):1:+
087600060612     c                                              LunDato_sk(sk))
087700060612     c                   End
087800060612      **
087900060612      * ? Nel 8° campo : 2° Nome Destinatario                        DNAME2
088000060612      **                                                             NOME 2 DESTINATARIO
088100060612     C                   add       1             sk
088200060612     c                   If        LunDato_sk(sk) >  0
088300060612     c                   eval      i1DNAME2   = %subst(Dato_sk(sk):1:+
088400060612     c                                              LunDato_sk(sk))
088500060612     c                   End
088600060612      **
088700060612      * ? Nel 9° campo : Nome Contatto Destinatario                  DCONTACT
088800060612      **                                                             NOME CONTATTO
088900060612     C                   add       1             sk
089000060612     c                   If        LunDato_sk(sk) >  0
089100060612     c                   eval      i1DCONTACT = %subst(Dato_sk(sk):1:+
089200060612     c                                              LunDato_sk(sk))
089300060612     c                   End
089400060612      **
089500060612      * ? Nel 10° campo : Indirizzo Ordinante        (Oblligatorio)  DSTREET
089600060612      **                                                             INDIRIZZO DESTINATARIO
089700060612     C                   add       1             sk
089800060612     c                   If        LunDato_sk(sk) =  0
089900060612      * ?-----> Errore
090000060614     c                   eval      tipo_error = 1
090100060614     c                   exsr      Field_Error
090200060612     c                   Else
090300060612     c                   eval      i1DSTREET  = %subst(Dato_sk(sk):1:+
090400060612     c                                              LunDato_sk(sk))
090500060612     c                   End
090600060612      **
090700060612      * ? Nel 11° campo : Nazione Destinatario       (Oblligatorio)  DCOUNTRYN
090800060612      **                                                             NAZIONE DESTINATARIO
090900060612     C                   add       1             sk
091000060612     c                   If        DatoNum_sk(sk) = 'N' or
091100060612     c                             LunDato_sk(sk) =  0
091200060612      * ?-----> Errore
091300060614     c                   eval      tipo_error = 2
091400060614     c                   exsr      Field_Error
091500060612     c                   Else
091600060612     c                   eval      i1DCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
091700060612     c                   End
091800060612      **
091900060612      * ? Nel 12° campo : Cod.Postale Destinatario   (Oblligatorio)  DPOSTAL
092000060612      **                                                             CAP ORDINANTE
092100060612     C                   add       1             sk
092200060612     c                   If        LunDato_sk(sk) =  0
092300060612      * ?-----> Errore
092400060614     c                   eval      tipo_error = 1
092500060614     c                   exsr      Field_Error
092600060612     c                   Else
092700060612     c                   eval      i1DPOSTAL  = %subst(Dato_sk(sk):1:+
092800060612     c                                              LunDato_sk(sk))
092900060612     c                   End
093000060612      **
093100060612      * ? Nel 13° campo : Città Destinatario         (Oblligatorio)  DCITY
093200060612      **                                                             CITTÀ DESTINATARIO
093300060612     C                   add       1             sk
093400060612     c                   If        LunDato_sk(sk) =  0
093500060612      * ?-----> Errore
093600060614     c                   eval      tipo_error = 1
093700060614     c                   exsr      Field_Error
093800060612     c                   Else
093900060612     c                   eval      i1DCITY    = %subst(Dato_sk(sk):1:+
094000060612     c                                              LunDato_sk(sk))
094100060612     c                   End
094200060612      **
094300060612      * ? Nel 14° campo : Provincia                                  DPROVINCE
094400060612      **                                                             PROVINCIA DESTINATARIO
094500060612     C                   add       1             sk
094600060612     c                   If        LunDato_sk(sk) >  0
094700060612     c                   eval      i1DPROV = %subst(Dato_sk(sk):1:+
094800060612     c                                              LunDato_sk(sk))
094900060612     c                   End
095000060612      **
095100060612      * ? Nel 15° campo : Cod ISO NAZIONE Provenienza                TCOUNTRYN
095200060612      **                                                             PAESE DI PROVENIENZA
095300060612     C                   add       1             sk
095400060612     c                   If        DatoNum_sk(sk) = 'N' and
095500060612     c                             LunDato_sk(sk) >  0
095600060612      * ?-----> Errore
095700060614     c                   eval      tipo_error = 3
095800060614     c                   exsr      Field_Error
095900060612     c                   Else
096000060612     c                   eval      i1TCOUNTRY =  atoi(%TRIM(Dato_sk(sk)))
096100060612     c                   End
096200060612      **
096300060612      * ? Nel 16° campo : Cod.Postale Destinatario di Provenienza    TPOSTAL
096400060612      **                                                             CAP DI PROVENIENZA
096500060612     C                   add       1             sk
096600060612     c                   If        LunDato_sk(sk) >  0
096700060612     c                   eval      i1TPOSTAL  = %subst(Dato_sk(sk):1:+
096800060612     c                                              LunDato_sk(sk))
096900060612     c                   End
097000060612      **
097100060612      * ? Nel 17° campo : Telefono Destinatario                      DPHONE
097200060612      **                                                             TEL DESTINATARIO
097300060612     C                   add       1             sk
097400060612     c                   If        LunDato_sk(sk) >  0
097500060612     c                   eval      i1DPHONE  = %subst(Dato_sk(sk):1:+
097600060612     c                                              LunDato_sk(sk))
097700060612     c                   End
097800060612      **
097900060612      * ? Nel 18° campo : FAX      Destinatario                      DFAX
098000060612      **                                                             TEL DESTINATARIO
098100060612     C                   add       1             sk
098200060612     c                   If        LunDato_sk(sk) >  0
098300060612     c                   eval      i1DFAX   = %subst(Dato_sk(sk):1:+
098400060612     c                                              LunDato_sk(sk))
098500060612     c                   End
098600060612      **
098700060612      * ? Nel 19° campo : E-MAIL   Destinatario                      DEMAIL
098800060612      **                                                             E-MAIL DESTINATARIO
098900060612     C                   add       1             sk
099000060612     c                   If        LunDato_sk(sk) >  0
099100060612     c                   eval      i1DEMAIL = %subst(Dato_sk(sk):1:+
099200060612     c                                              LunDato_sk(sk))
099300060612     c                   End
099400060612      **
099500060612      * ? Nel 20° campo : AZIENDA                    (Oblligatorio)  COMPANY
099600060612      **                                                             ID.AZIENDA
099700060612     C                   add       1             sk
099800060612     c                   If        LunDato_sk(sk) =  0
099900060612      * ?-----> Errore
100000060614     c                   eval      tipo_error = 1
100100060614     c                   exsr      Field_Error
100200060612     c                   Else
100300060612     c                   eval      i1COMPANY  = %subst(Dato_sk(sk):1:+
100400060612     c                                              LunDato_sk(sk))
100500060612     c                   End
100600060612      **
100700060612      * ? Nel 21° campo : GEOID                                      GEOID
100800060612      **                                                             COD.ASCII 037
100900060621     C                   clear                   i1GEOID_A         1
101000060612     C                   add       1             sk
101100060612     c                   If        DatoNum_sk(sk) = 'N' and
101200060612     c                             LunDato_sk(sk) >  0
101300060612      * ?-----> Errore
101400060614     c                   eval      tipo_error = 3
101500060614     c                   exsr      Field_Error
101600060612     c                   Else
101700060612     c                   eval      i1GEOID =  atoi(%TRIM(Dato_sk(sk)))
101800060612
101900060612     C                   move(p)   i1GEOID       §xteAdec
102000060612      **   Convertitore
102100060612     c                   call      'XTOEBCDR'
102200060612     c                   parm                    xtoEBCDds
102300060612     C                   IF        §XTEERR <> *blanks
102400060612      **  Errore di Conversione
102500060612      * ?-----> Errore
102600060614     c                   eval      tipo_error = 4
102700060614     c                   exsr      Field_Error
102800060718     C                   move      '%'           i1GEOID_A         1
102900060612     C                   ELSE
103000060718      * se il valore era "000" ossia non era arrivato da DPD impostiamo default "%"
103100060718     c                   if        §xteAchr = 'NUL'
103200060718     C                   move      '%'           i1GEOID_A
103300060718     c                   else
103400060612     C                   move      §xteAchr      i1GEOID_A
103500060718     c                   end
103600060612     C                   ENDIF
103700060612     c                   End
103800060612      **
103900060612      * ? Nel 22° campo : Numero Mezzo CMR           (Oblligatorio)  REMARK
104000060612      **                                                             LORRY NUMBER
104100060612     C                   add       1             sk
104200060612     c                   If        LunDato_sk(sk) =  0
104300060612      * ?-----> Errore
104400060614     c                   eval      tipo_error = 1
104500060614     c                   exsr      Field_Error
104600060612     c                   Else
104700060612     c                   eval      i1REMARK   = %subst(Dato_sk(sk):1:+
104800060612     c                                              LunDato_sk(sk))
104900100105      *
105000100105      * se non c'è il CMR imposta la HUB di provenienza
105100100105     c                   if        i1REMARK = *blank
105200100105     c                   Eval      i1REMARK = %Trim(i1sdepot)
105300100105     c                   End
105400100105      *
105500060612     c                   End
105600060612      **
105700060612      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL TIDP100W
105800060612      **
105900060612     c                   Endsr
106000060614      * ?------------------------------------------------------------------ */
106100060614      *?      Campi in errore
106200060614      * ?------------------------------------------------------------------ */
106300060614     c     Field_Error   Begsr
106400060614      **
106500060614     C                   eval      se_errore   = 'S'
106600060614      * msg video o in posta
106700060614     c                   If        Msg_Err = *blank
106800060614     c
106900060614     c                   Select
107000060614     c
107100060614     c                   When      Tipo_Error = 1
107200060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
107300060614     C                             ' >> Obblig.- Manca Dato !!'
107400060614     c
107500060614     c                   When      Tipo_Error = 2
107600060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
107700060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
107800060614     c
107900060614     c                   When      Tipo_Error = 3
108000060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
108100060614     C                             ' >> NON Numerico  !!'
108200060614     c
108300060614     c                   When      Tipo_Error = 4
108400060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
108500060614     C                             ' >> Conversione non riuscita !!'
108600140827     c
108700140827      * non essendo un campo ma tutta la riga l'indice di skiera è sposizionato
108800140827      * NON deve quindi eseguire EVAL del "Msg_Vin_80" seguente xché si
108900140827      * spaccherebbe per indice di schiera NON valido.
109000140827     c                   When      Tipo_Error = 5
109100140827     C                   eval      Msg_Err = 'Campi riga sposizionati >>' +
109200140827     C                             ' Spedizione NON tradotta !!'
109300140827     C                   eval      Msg_Vin_80 = Msg_Err
109400060614     c                   EndSL
109500060614     c
109600060614     c                   End
109700060614      * Errore su VINMSG
109800140827     c                   if        Tipo_Error <> 5
109900060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
110000060614     c                                          + %trim(Testa_sk(sk))
110100140827     c                   End
110200060614      **
110300060614     c                   Endsr
110400060612      * ?------------------------------------------------------------------ */
110500060612      *?      Suddivide i campi della riga con carattere divisorio (;)
110600060612      * ?------------------------------------------------------------------ */
110700060612     c     Split_Rec     Begsr
110800060612      **
110900060612     c                   clear                   Nr_campi
111000060612     c                   clear                   Campi_Record
111100060612     c                   clear                   Campi_Lunghi
111200060612     c                   clear                   Campi_Numerici
111300060612     c                   clear                   Campi_Decimali
111400060612     c                   movel     ';'           Separa_campi
111500060612     c                   movel     '.'           Decimal_Separ
111600060612      *
111700060612     c                   call      'FIEU00R'
111800060612      * ?     Input
111900060612     c                   parm                    Separa_campi
112000060612     c                   parm                    Decimal_Separ
112100060612     c                   parm                    dati
112200060612      * ?     Output  su schiere di 100 elementi
112300060612     c                   parm                    Nr_campi
112400060612     c                   parm                    Campi_Record                   * i dati
112500060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
112600060612     c                   parm                    Campi_Numerici                 * se numerici
112700060612     c                   parm                    Campi_Decimali                 * quanti decimali
112800060612      **
112900060612     c                   Endsr
113000160120      * ?================================================================== */
113100160120     C*?  ERA GIA' su EDIVAB perchè scritto da GEODATA
113200160120      * ?================================================================== */
113300160120     C     UPD_DA_GEO    BEGSR
113400160120      *
113500160120     c                   clear                   new_CMR          35
113600160120      *
113700160120     c                   eval      new_CMR  = %trim(i1REMARK)+'_('+
113800160120     c                             %trim(i1RDEPOT) + ')_'+
113900160120     c                             %trim(Depot_TRS_UNO)
114000160120      *  Solo se ci sta aggiungo la data
114100160120     c                   clear                   lungo_size        3 0
114200160120     c                   eval      lungo_size = %len(%Trim(new_CMR))
114300160120     c                   if        lungo_size < 30
114400160120     c                   z-add     DATEU         DATA4             4 0
114500160120     c                   EVAL      new_CMR = %trim(new_CMR) + '_' +
114600160120     c                             %trim(%editw(DATA4: '0 /  '))
114700160120     c                   end
114800160120      *
114900160120      *  per chiave bolla aggiorna la bolla in SOSPENZIONE che adesso
115000160120      *  risulta partita con l'IGATEAVIS ricevuto (Manifest)
115100160120     C     KSosp_EDIVA   setll     EDIVAT3L
115200160120     C     KSosp_EDIVA   reade     EDIVAT3L
115300160120     c                   DOw       not %EoF(EDIVAT3L)
115400160120     c                   eval      vatCMR  =  new_CMR
115500160120     c                   Update    EDIVAT00
115600160120     C     KSosp_EDIVA   reade     EDIVAT3L
115700160120     c                   EndDO
115800160120      *
115900160120      *  poi allinea anche la testata bolla del VAB
116000160120     C     KSosp_EDIVA   chain     EDIVAB2L
116100160120     c                   if        %Found(EDIVAB2L)
116200160120     c                   eval      vabCMR  =  new_CMR
116300160208      *** Provenienza
116400160208     c                   movel     'X'           VABscl
116500160120     c                   Update    EDIVAB2
116600160120     c                   End
116700160120      *
116800160120     c                   Endsr
116900060612      * ?================================================================== */
117000060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
117100060612      * ?================================================================== */
117200060612     C     WRT_VAB_VAT   BEGSR
117300060614      *
117400060612      * Clearizzo EDiVAB in quanto ha gli stessi campi di FiVAB più
117500060612      *  alcuni specifici
117600060612     c                   clear                   edivab00
117700060612     c                   clear                   fivab000
117800060621     c                   clear                   err_bloccante     1
117900100105     c                   clear                   Sped_RESO         1
118000060614      *
118100060612      **?------------------------------------------------------------------ */
118200060612      *   In base alla HUB di arrivo determino la Filiale di gestione
118300060612      *?   Il tutto è pilotato dalla tabella "DEP"
118400060612      **?------------------------------------------------------------------ */
118500060612      * ?Dal Depot DPD prendo il relativo cliente e LNP se su tab.:DEP
118600060612     c                   z-add     1             xx
118700060612     c                   clear                   su_EDIVAB         1
118800091106     c                   movel(P)  i1SDEPOT      W015A
118900091106     c                   movel(P)  W015A         W030A
119000091106     c                   moveL(P)  i1RDEPOT      W015A
119100091106     c                   move      W015A         W030A
119200060913     c                   clear                   depotK            4
119300150604     c                   clear                   codtarVTG         3 0
119400091106      *
119500091106      * completo con HUB di arrivo
119600091106     c     W030A         lookup    DEPK2(xx)                              31
119700091109     c  n31              clear                   W015A
119800091106      * senza HUB di arrivo
119900091106     c  n31              move      W015A         W030A
120000091106     c  n31W030A         lookup    DEPK2(xx)                              31
120100091106      *
120200091106     c                   if        *in31
120300060620      **
120400060913     c                   movel     DepK(xx)      depotK
120500060612     c                   movel     DepC(xx)      vabCCM
120600060612     c                   movel     DepL(xx)      vabLNP
120700060612     c                   movel     DepL(xx)      vabFGS
120800060612     c                   movel     DepM(xx)      vabCTM
120900060612     c                   movel     DepT(xx)      vabCTR
121000150604     c                   movel     DepTvtg(xx)   codtarVTG         3 0
121100060612     c                   movel     DepW(xx)      su_EDIVAB
121200160301      **
121300160301     c                   movel     'LPD'         tbecod
121400160301     c                   movel(p)  vablnp        tbeke1
121500160301     c     KeyTBE        chain     tntbe01l
121600160301     c                   if        %Found(tntbe01l)
121700160301     c                   eval      dLPD = tbeUNI
121800160301     c                   If        §LPDwriIGA = 'N'
121900160301     c                   eval      se_errore ='S'
122000160301     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80) + ';' +
122100160301     c                             'Salta write x tab.LPD-> NO IGATEAVIS'
122200160301     c                   leaveSR
122300160301     c                   end
122400160301     c                   else
122500160301     c                   end
122600160301      **
122700060612     c                   else
122800060612      **
122900060612      *?   SE è arrivato sul file un HUB non presente in tabella "DEP" invia messaggio
123000060612      * ?Se è arrivata un Depot/HUB non configurato x la ricezione
123100060612      * ?   AVVISA mediante msg utente e Posta AS400 della mancata ricezione
123200060612      **
123300060614     C                   eval      se_errore   = 'S'
123400060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
123500060614     c                                          + 'TAB:DEP ' + %trim(i1SDEPOT)
123600091106     c                                          + '/' + %trim(i1RDEPOT)
123700091106     c                                          + '_Mancante'
123800060621     c                   move      'S'           err_bloccante
123900120504      *
124000120504      *  Attivato xchè importante inviare un MSG
124100120504      *
124200120504     c                   if        non_inviato_prima = *blank
124300120514     C                   movel     ' '           Msg_Vin_80
124400120514     C                   eval      wrkMsg = Msg_Vin_80
124500120514     C                   eval      wrkMsg = %trim(wrkMSG) + ' inserire su tabel+
124600120514     c                             la DEP (TNTBE00F) il nuovo codice DEPOT DPD +
124700120514     c                             x poter ricevere i dati. Dopo avere inserito+
124800120514     c                              la tabella rieseguire l''UPLOAD di questo f+
124900120514     c                             ile.  +
125000120514     c                             Avvisare   Andrea o Nicola.'
125100120504     C                   exSR      eMail_alert
125200120504     c                   eval      non_inviato_prima = 'S'
125300120504     c                   end
125400120504      *
125500060614     c                   goto      Error_VAB
125600060612     c                   end
125700060612      **?------------------------------------------------------------------ */
125800060612      * ?   ( Il parcel non è + numerico )
125900060612     c
126000060612      * ?il Parcel deve essere impostato nella DS VATE
126100060612      * e viene impostato
126200060612      * fisso il rif.numerico a (1)
126300060710      *  se comunque il Parcel ricevuto è numerico impostiamo nel riferimento
126400060710      *  numerico il nr.Parcel come avveniva prima che diventasse alfa il campo
126500060710     C                   z-add     1             vabrmn
126600060710      **
126700060710     c     digits        check     i1PARCEL      position
126800060710      **
126900060710      **  presenti campi non numerici: (Ossia trovati)
127000060710     c                   iF        %Found
127100060911      * ?  per impostare il riferimento numerico (VABRMN):
127200060710      **    è importante che siano oltre il limite della lunghezza del Parcel
127300060710      **    in tal caso sono solo i blanks alla destra del numero
127400060710      **
127500060710     c                   if        Lung_Parcel = 14 and position > 14
127600060710     C                   MOVEL     i1PARCEL      parcel14
127700060710     c                   move      parcel14      vabrmn
127800060710     c                   end
127900060710      **
128000060710     c                   elsE
128100060710      **
128200060710     c                   if        Lung_Parcel = 14
128300060710     C                   MOVEL     i1PARCEL      parcel14
128400060710     c                   move      parcel14      vabrmn
128500060710     c                   end
128600060710      **
128700060710     c                   enD
128800150604      **
128900150604      ** imposta il Depot di provenienza x testare se VTG
129000150604     c                   movel     parcel14      Depot04
129100150604     c                   clear                   parcel_VTG        1
129200150604      **
129300150604     C                   movel(p)  'VTG'         cldCOD
129400150604     C                   move      Depot04       cldDEP
129500150604     c     key_VTG       setll     tncld00F
129600150604     c     key_VTG       reade     tncld00F
129700150604     c                   dow       not %EoF(tncld00F)
129800150604     c                   if        dateu >= cldDDA and
129900150604     c                             dateu <= cldADA
130000150604     c                   move      'S'           parcel_VTG
130100150604     c                   leave
130200150604     c                   end
130300150604     c     key_VTG       reade     tncld00F
130400150604     c                   endDO
130500150604      **
130600150604      **  Se è un collo VTG deve avere una tariffa differente
130700150604     c                   if         parcel_VTG = 'S'
130800150604     c                   movel     codtarVTG     vabCTR
130900150604     c                   end
131000060612      * -----------------                  -------------------
131100060612      * ?Numero spedizione
131200060612     C                   EXSR      SUB_keybol
131300060612      *
131400060612      * ?Data  spedizione
131500060612     C                   MOVEL     UMONTH        VABMGS                         = oggi
131600060612     C                   MOVE      Uday          VABMGS                         = oggi
131700060612      *
131800060612      * ?Dati  destinatario
131900060612     c                   movel     i1DNAME1      vabrsd                         Ragione Sociale
132000060612     C                   movel     i1DNAME2      vabrd2                         rag.soc.2
132100060612     c                   movel     i1DSTREET     vabind                         Indirizzo
132200060612     c                   movel     i1DCITY       vablod                         Località
132300060612     c                   movel     i1DPOSTAL     vabcad                         C.A.P.
132400060612     c                   movel     i1DPROV       vabprd                         Provincia
132500070202      * ?Se manca la provincia la calcolo
132600070202     c                   if        vabprd = *blank
132700070202     C                   CLEAR                   TISI95DS
132800070202     C                   MOVEL     vabCAD        I95CAP
132900070202     C                   MOVEL     DATEU         I95DAT
133000070202     C                   MOVEL     '3'           I95TCN
133100070202     C                   CALL      'TISI95R'
133200070202     C                   PARM                    TISI95DS
133300070202      *     Errore
133400070202     C     O95ERR        IFeq      *BLANKS
133500070202     C                   MOVEL     O95PRV        vabPRD
133600070202     C                   END
133700070202      *
133800070202     c                   end
133900060612      * ?Tipo servizio
134000060612     c                   movel     'C'           vabtsp
134100060612      * ?Colli
134200060612     c                   z-add     1             vabncl
134300060612      *
134400060612      * ?Service  Code:  101-Normal Parcel
134500060612      * ?                136-Small Parcel
134600060612      * ?                298-Return Sender
134700060612      * ?                300-Return
134800111110      * ?                327-Normal Parcel B2C
134900111110      * ?                329-Normal Parcel B2C
135000111110      * ?                328-Small  Parcel B2C
135100140409      * ?                365-Tyres
135200140409      * ?                366-Tyres B2C
135300140409      * ?                367-Tyres COD
135400060621     c                   clear                   w015A
135500060612     c                   if        i1SERVICE = 101
135600111110      *******  da eliminare in seguito
135700111110     c                               or
135800111110     c                             i1SERVICE = 327 or i1SERVICE = 329
135900111110      *******
136000060612     c                   eval      W015A = 'PESO01'
136100060612     c                   end
136200141210      *******
136300141210      ** aggiunto per il traffico gomme.
136400141210     c                   if        i1SERVICE = 365
136500141210     c                               or
136600141210     c                             i1SERVICE = 366
136700141210     c                               or
136800141210     c                             i1SERVICE = 367
136900141210     c                   eval      W015A = 'PESO03'
137000141210     c                   end
137100140409      *
137200060612     c                   if        i1SERVICE = 136
137300111110      *******  da eliminare in seguito
137400111110     c                               or
137500111110     c                             i1SERVICE = 328
137600111110      *******
137700060612     c                   eval      W015A = 'PESO02'
137800060612     c                   end
137900130819      ***
138000130819      * se per qualche motivo non passano il codice pacco/peso imposto come
138100130819      *   pacco grande
138200130819     c                   if        W015A = *blank
138300130819     c                   eval      W015A = 'PESO01'
138400130819     c                   end
138500130819      *
138600060612      * ?Peso, ricavato dal tipo servizio del ROUTELABEL
138700060612     c                   z-add     1             xx
138800060621     c                   if        w015a <> *blank
138900060612     c     W015A         lookup    DPPK(xx)                               31
139000060612     c                   If        *in31 = *off
139100060612     c                   eval      W015A = 'PESO'
139200060612     c     W015A         lookup    DPPK(xx)                               31
139300060612     c                   Endif
139400060612     c                   z-add     DPP(xx)       vabPKB
139500161006     c                   z-add     vlb(xx)       vabVLB
139600060621     c                   End
139700060612      *
139800060612      * ?Codice bolla
139900060612     C                   MOVEL     '1'           vabcbo                         NO C/assegno
140000060612      *
140100060612      * ?Se spedizione RESA imposto diversamente il VAB
140200060911      * ?   ma se DPD non invia correttamente il service Code
140300060911      * ?   identifichiamo il reso dal Depot Bartolini sul Parcel
140400060911      **
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
143100061016     c                   if        Lung_Parcel = 14
143200171009     C                   MOVEL(p)  parcel14      dpo_Parcel       14
143300061016     c                   end
143400061016      * ?Controlla sul FIDPO x metterlo in assegnato
143500061016     C     dpo_Parcel    chain     fidpo06l
143600061016     c                   if        %Found(fidpo06l)
143700061016      *assegnato
143800061016     C                   MOVEL     '2'           vabcbo
143900061016     c                   else
144000061016      * ?Se non l'ha trovato potrebbe essere un Parcel Taroccato ossia lungo 14
144100061016      * ? ma con 0 e 99 in mezzo quindi ritento l'aggancio.
144200061016     c                   if        Lung_Parcel = 14
144300061016     C                   MOVEL     i1PARCEL      dsparcel
144400061016     c                   if        Parcel0 = '0' and NoveNove ='99'
144500121115      *  NON deve più farlo
144600121115     c                   goto      vecchioPRCL
144700061016      *  Deve reimpostare a 11 il Parcel
144800061016     c                   clear                   dpo_Parcel
144900061016     c                   movel     Depot3        parcel11a        11
145000061016     c                   move      NumParcel     parcel11a
145100061016     C                   MOVEL(p)  parcel11a     dpo_Parcel       14
145200061016     C     dpo_Parcel    chain     fidpo06l
145300061016     c                   if        %Found(fidpo06l)
145400061016      *assegnato
145500061016     C                   MOVEL     '2'           vabcbo
145600061016     c                   end
145700121115     c     vecchioPRCL   tag
145800061016     c                   end
145900061016     c                   end
146000061016      *
146100061016     c                   end
146200061016      *
146300100920      * ?SE si tratta di un ORM commissionato a DPD, Controllo se deve essere
146400100920      * ?in FERMO DEPOSITO.
146500100920     c                   eval      vabFFD = *blank
146600100920     C                   if         vabcbo = '2'  and dpoPOE > 0
146700100920      *
146800100920     C                   z-add     dpoPOE        ORMPOE
146900100920     C                   z-add     dpoNRS        ORMNSR
147000100920     C                   z-add     dpoNOR        ORMNOR
147100100920     C                   z-add     dpoNRV        ORMNRV
147200100920      * ?lo vedo sull'ORM
147300100920     C     KORM          chain     fnORM01l
147400100920     c                   if        %Found(FnOrm01l) and ormffd ='S'
147500100920     c                   eval      vabFFD = 'S'
147600100920     c                   end
147700100920     c                   end
147800101112      *
147900141210     c********** ELIMINATO e GESTITO con SERVICE CODE GOMME e tabella "PESO03"
148000101112      *  Chiodo per la linea 193 solo merce ricevuta dal Depot '0130'
148100101112      *  sono camion di Gomme che devono avere peso medio di 19 Kg invece che 11 Kg
148200101112      *  e volume 0,12 mc anzichè 0,055 mc.
148300141210     c**********         if          vablnp = 193 and i1SDEPOT = '0130'
148400141210     c**********         z-add     19            vabPKB
148500141210     c**********         z-add     0,12          vabVLB
148600141210     c**********         end
148700151030      *** Provenienza
148800151030     c                   movel     'I'           VABscl
148900151113      *
149000151113      * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
149100100920      *
149200151113      * %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
149300151113      * ?==============================
149400151113      * ?==============================
149500060612      * ?Scrivo EDiVAB o FiVAB
149600151113      * ?==============================
149700151113      * ?==============================
149800060612     C                   If        su_EDIVAB = 'S'
149900060612      ***
150000091223      *
150100100105      * ?      COMPONE IL CMR per identificare il collo NELLA CASSA/Trasmissione
150200100105     c                   if        Sped_Reso = ' '
150300111010      **********
150400111010     c                   eval      i1REMARK = %trim(i1REMARK)+'_('+
150500111010     c                             %trim(i1RDEPOT) +
150600111010     c                             ')_'+ %trim(Depot_TRS_UNO)
150700111010      *
150800100105      * se trattasi di Reso (Evidenzio)
150900100105     c                   ElseIf    Sped_Reso = 'S'
151000100105     c                   movel(p)  i1REMARK      campo35          35
151100100105     c                   movel(p)  'RESI_'       i1REMARK
151200100107     c                   eval      i1REMARK = %trim(i1REMARK) +
151300100107     c                                        %trim(campo35)
151400100105      *
151500100105     c                   eval      i1REMARK = %trim(i1REMARK) +
151600100105     c                             '_('+ %trim(i1RDEPOT) +
151700100105     c                             ')_'+ %trim(Depot_TRS_UNO) +':' +
151800100105     c                             %trim(%editc(Num_Trs_UNO:'Z'))
151900100105     c                   end
152000090629      *
152100090629      *  Solo se ci sta aggiungo la data
152200090629     C                   move      i1REMARK      VABcmr
152300090629      *
152400090629     c                   clear                   lungo_size        3 0
152500090629     c                   eval      lungo_size = %len(%Trim(VABCMR))
152600090629     c                   if        lungo_size < 30
152700090629     c                   z-add     DATEU         DATA4             4 0
152800090629     c                   EVAL      VABCMR = %trim(VABCMR) + '_' +
152900090629     c                             %trim(%editw(DATA4: '0 /  '))
153000090629     c                   end
153100100315      *
153200060612     c                   z-add     DATEU         VABdcm
153300060612     c                   z-add     DATEU         VABdts
153400060612     c                   movel     WORA          VABhms
153500150908      ***
153600060612     c                   write     EDivab00
153700060612     c                   exsr      wrtvat
153800060612      *
153900060612     c                   ELSE
154000060612      ***
154100060612     c                   write     FiVAB000
154200060612     c                   exsr      wrtvat
154300060612      ***
154400060612     c                   ENDIF
154500060614      ***
154600060614     c                   move      'S'           Almeno_Uno
154700060612      *
154800060614     c     Error_VAB     Endsr
154900060612     C*----------------------------------------------------------------
155000060612     C*? ESEGUO SCRITTURA EDiVAT Riferimenti x il destinatario
155100060612     C*----------------------------------------------------------------
155200060612     C     WRTVAT        BEGSR
155300060612      *
155400060413      * ? Parcel Nr. --> Riferimento Parcel DPD
155500051219     C                   If        su_EDIVAB = 'S'
155600041215     c                   clear                   edivat00
155700041215     c                   else
155800041215     c                   clear                   fivat000
155900041215     c                   end
156000041215      **
156100041215     C                   movel     VABfgs        VATfgs
156200041215     C                   Z-ADD     VABCCM        VATCCM
156300041215     C                   Z-ADD     VABAAS        VATAAS
156400041215     C                   Z-ADD     VABLNP        VATLNP
156500041215     C                   Z-ADD     VABNRS        VATNRS
156600041215     C                   Z-ADD     VABNSP        VATNSP
156700060413      **
156800060608     C                   MOVEL     'E'           VATTRC
156900060612      **
157000060612      **  su nuovo tipo record (E)
157100060608     c                   clear                   DVate
157200060621      *
157300060621      *  IMPOSTA la DS VAT(E)
157400060612     C                   MOVEL     i1PARCEL      §VATEPRN
157500060620     C                   move      *zeros        §VATESCD
157600060620     C                   move      i1SERVICE     §VATESCD
157700060612     C                   move(p)   i1GEOID_A     §VATEBID
157800171009      *
157900171009     C                   MOVEL     *blank        §VATESDEP
158000171009     C                   MOVE      I1SDEPOT      §VATESDEP
158100060620      *
158200171009      * trascodifico la B.U.
158300171009      *
158400171009      * recupero  versione di cappario
158500171009     C                   clear                   ISID1DRI
158600171009     c                   z-add     dateu         ISID1DRI
158700171009     C                   clear                   OSID1VER
158800171009     C                   clear                   OSID1VERD
158900171009     C                   clear                   OSID1DDE
159000171009     C                   clear                   OSID1DSC
159100171009     C                   clear                   OSID1BCID
159200171009     c                   call      'TISID1R'
159300171009     C                   PARM                    ISID1DRI
159400171009     C                   PARM                    OSID1VER
159500171009     C                   PARM                    OSID1VERD
159600171009     C                   PARM                    OSID1DDE
159700171009     C                   PARM                    OSID1DSC
159800171009     C                   PARM                    OSID1BCID
159900171009      * se TROVATA VERSIONE CAPPARIO
160000171009      * prova a decodificare il DEPOT con il nuovo cappario per reperire la BU
160100171009     c                   if        osid1ver > 0
160200171009      **
160300171009     c                   move      *blank        CDPATB
160400171009     c                   z-add     OSID1VER      CDPVER
160500171009     c                   movel     I1SDEPOT      CDPDSTR
160600171009     c     Kcdp4         chain     dpcdp12i
160700171009     c                   if        %Found(dpcdp12i)
160800171009      * si sovrappone a quanto pre impostato
160900171009      *    altrimenti il Depot non avrà la B.U.
161000171009     c                   movel     CDPDPTN       §VATESDEP
161100171009     c                   end
161200171009     c                   end
161300171009      ** ------
161400060608     C                   MOVEL     DVate         VATNOT
161500041215      **
161600051219     C                   If        su_EDIVAB = 'S'
161700041215     C                   MOVEL     VABCMR        VATCMR
161800041215     C                   Z-ADD     VABCNT        VATCNT
161900041215     C                   WRITE     EDiVAT00
162000041215     c                   else
162100041215     C                   WRITE     fiVAT000
162200041215     c                   end
162300060413      *
162400060413      * ? Contatto del Destinatario
162500060612     C                   if        i1DCONTACT <> *Blanks
162600060413     C                   If        su_EDIVAB = 'S'
162700060413     c                   clear                   edivat00
162800060413     c                   else
162900060413     c                   clear                   fivat000
163000060413     c                   end
163100060413      **
163200060413     C                   movel     VABfgs        VATfgs
163300060413     C                   Z-ADD     VABCCM        VATCCM
163400060413     C                   Z-ADD     VABAAS        VATAAS
163500060413     C                   Z-ADD     VABLNP        VATLNP
163600060413     C                   Z-ADD     VABNRS        VATNRS
163700060413     C                   Z-ADD     VABNSP        VATNSP
163800060413      **
163900060413     C                   MOVEL     'A'           VATTRC
164000060612     C                   MOVEL     i1DCONTACT    VATNOT
164100060413      **
164200060413     C                   If        su_EDIVAB = 'S'
164300060413     C                   MOVEL     VABCMR        VATCMR
164400060413     C                   Z-ADD     VABCNT        VATCNT
164500060413     C                   WRITE     EDiVAT00
164600060413     c                   else
164700060413     C                   WRITE     fiVAT000
164800060413     c                   end
164900060413     c                   endIF
165000041215      *
165100060413      * ? Contatto Telefonico
165200060612     C                   if        i1DPHONE   <> *Blanks
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
166600060413     C                   MOVEL     'B'           VATTRC
166700060612     C                   MOVEL     i1DPHONE      VATNOT
166800060413      **
166900060413     C                   If        su_EDIVAB = 'S'
167000060413     C                   MOVEL     VABCMR        VATCMR
167100060413     C                   Z-ADD     VABCNT        VATCNT
167200060413     C                   WRITE     EDiVAT00
167300060413     c                   else
167400060413     C                   WRITE     fiVAT000
167500060413     c                   end
167600060413     c                   endIF
167700060413      *
167800060413      *
167900041215     C                   ENDSR
168000971215      *----------------------------------------------------------------
168100971216      * DETERMINO DATA E CHIAVE SPEDIZIONE
168200971215      *----------------------------------------------------------------
168300991206     C     SUB_keybol    BEGSR
168400050112      *
168500050112      **              **------------------**
168600050112      *    deve controllare sulla tabella "3C" se il numero spedizione
168700050112      *     deve essere mantenuto oppure no
168800050112     C                   clear                   Ds3C
168900060620     C                   Z-ADD     1             TBLKUT
169000050112     C                   MOVEL     '3C'          TBLCOD
169100050112     C                   movel(p)  VABccm        TBLKEY
169200050112     C     KTABel        CHAIN     TABEL00F                           30
169300050112     C                   IF        %Found(TABEL00F) and tblflg = *blank
169400050112     C                   MOVEL     TBLUNI        Ds3C
169500050112     C                   END
169600050112      *
169700050112      *   se non deve essere mantenuto lo prende dal nuovo numeratore Bolle VAB
169800050112     c                   if        §3CFsp <> 'S'
169900050112      *
170000050112     C* NSP => Stacco un numeratore da AZNUM
170100050112     c                   movel     kpjbu         svkpjbu
170200050112     c                   clear                   kpjbu
170300050112     C                   clear                   TRUL33DS
170400050112     C                   eval      I33OPE = *zeros
170500050112     C                   eval      I33CNU = 302
170600050112     C                   eval      I33NUM = 1
170700050112     C                   movel     TRUL33DS      KPJBU
170800050112     C                   call      'TRUL33R'
170900050112     C                   parm                    KPJBA
171000050112     C                   movel     KPJBU         TRUL33DS
171100050112     C                   if        O33ERR = *zeros
171200050112     c                   z-add     o33nrf        NUM_SPED
171300050112     C                   endif
171400050112     c                   movel     svkpjbu       kpjbu
171500050112      ****
171600050112     c                   ELSE
171700050112      *   se si vuole mantenere il numero spedizione
171800050112      ****
171900050112     C                   MOVE      UYEAR         KAAA
172000050112     C                   Z-ADD     3             KCNU
172100050112     C                   Z-ADD     VABLNP        KFIL
172200050112     C     KNUF          CHAIN     FLNUF                              9999
172300050112     C                   IF        *IN99 = *OFF
172400050112     C                   ADD       1             NUFNUM
172500050112     C                   Z-ADD     NUFNUM        NUM_Sped                       *NUM.SPEDIZIONE
172600050112     C                   UPDATE    FLNUF
172700060612     c                   END
172800060411      *
172900050112     c                   EndIF
173000050112      **
173100050112      **              **------------------**
173200050112     C                   Z-ADD     NUM_Sped      VABNSP                         *NUM.SPEDIZIONE
173300050112      **              **------------------**
173400971215      *
173500050112     C                   MOVE      UYEAR         KAAA
173600991206     c                   IF        KAAA > 90
173700991206     C     1900          add       kaaa          VABAAS
173800991206     C                   ELSE                                                   data sped.
173900991206     C     2000          add       kaaa          VABAAS
174000991206     c                   ENDIF                                                  data sped.
174100971216      *
174200991206     C                   ENDSR
174300060620      *----------------------------------------------------*
174400060620      ** CALCOLA CHECK DIGIT -----------------------------
174500060620      *----------------------------------------------------*
174600060620     C     M1031         BEGSR
174700060620      *
174800060620     C                   MOVE      *ZERO         §TT2              3 0
174900060620     C                   CLEAR                   W0030             3 0
175000060620     C                   CLEAR                   W0010             1 0
175100060620      *
175200060620      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
175300060620     C                   DO        11            YY                2 0
175400060620      *
175500060620     C     YY            DIV       2             §CTR              3 0
175600060620     C                   MVR                     §RST              3 030
175700060620      *
175800060620     C     *IN30         IFEQ      '1'
175900060620     C     §11(YY)       MULT      3             §CTR
176000060620     C                   ELSE
176100060620     C                   Z-ADD     §11(YY)       §CTR
176200060620     C                   ENDIF
176300060620      *
176400060620     C                   ADD       §CTR          W0030
176500060620     C                   ENDDO
176600060620      *
176700060620     C     W0030         ADD       9             §TT2
176800060620     C                   MOVE      0             §TT2
176900060620     C     §TT2          SUB       W0030         DPDCHD
177000060620      *
177100060620     C                   ENDSR
177200051205      *----------------------------------------------------*
177300051205      *   DEFINIZIONE CHIAVI                               *
177400051205      *----------------------------------------------------*
177500051205     C     *INZSR        BEGSR
177600051205      *
177700991129     c     *ENTRY        PLIST
177800060613     C                   parm                    esito             1
177900110930     C                   eval      esito ='0'
178000991129      *
178100991206     C     KNUF          KLIST
178200991206     C                   KFLD                    KAAA
178300991206     C                   KFLD                    KCNU
178400991206     C                   KFLD                    KFIL
178500971215      *
178600050112     C     KTABel        KLIST
178700050112     C                   KFLD                    TBLKUT
178800050112     C                   KFLD                    TBLCOD
178900050112     C                   KFLD                    TBLKEY
179000100920      *
179100100920     C     KORM          KLIST
179200100920     C                   KFLD                    ORMPOE
179300100920     C                   KFLD                    ORMNSR
179400100920     C                   KFLD                    ORMNOR
179500100920     C                   KFLD                    ORMNRV
179600141103      *
179700141103     C     KBLP          KLIST
179800171020     C                   KFLD                    pndAAS
179900171020     C                   KFLD                    pndLNP
180000171020     C                   KFLD                    pndNRS
180100171020     C                   KFLD                    pndNSP
180200110930      *
180300171020     C***  Kar4I         KLIST
180400171020     C******             KFLD                    AR4TRC
180500171020     C******             KFLD                    AR4N30
180600100920      *
180700110930     C     Kvat4E        KLIST
180800110930     C                   KFLD                    vatTRC
180900110930     C                   KFLD                    vatNOT
181000150604      *
181100150604     C     key_VTG       KLIST
181200150604     C                   KFLD                    cldCOD
181300150604     C                   KFLD                    cldDEP
181400160120      *
181500160120     C     KSosp_EDIVA   KLIST
181600160120     C                   KFLD                    Sosp_AAS
181700160120     C                   KFLD                    Sosp_LNP
181800160120     C                   KFLD                    Sosp_NRS
181900160120     C                   KFLD                    Sosp_NSP
182000160301      **
182100160301     c     KeyTBE        klist
182200160301     c                   kfld                    tbecod
182300160301     c                   kfld                    tbeke1
182400171009      **
182500171009     c     Kcdp4         klist
182600171009     c                   kfld                    CDPATB
182700171009     c                   kfld                    CDPVER
182800171009     c                   kfld                    CDPDSTR
182900110930      *
183000050414      *------------------
183100020919      * Apro file transito o FiVAB pilotato dalla tabella
183200991124      * Caricamento Tabella PESI DPD
183300060620     c                   clear                   XX
183400991124     c                   clear                   DPP
183500991124     c     'DPP'         chain     tntbe000                           31
183600991124      *
183700991124     c                   DOW       *in31 = *off
183800991124      * Se il S.I. è indicato deve essere uguale al mio
183900991124     c                   IF        TBEatb = *BLANKS  and
184000991129     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
184100991124     c                   add       1             xx
184200991124     c                   movel     TBEke1        DPPK(XX)
184300000406     c                   movel     TBEuni        DDPP
184400000410     c                   movel     §DPPpkb       DPP(XX)
184500141210     c                   movel     §DPPvlb       vlb(XX)
184600991124     c                   endif
184700991124     c     'DPP'         reade     tntbe000                               31
184800991124     c                   enddo
184900971216      *
185000000908      * Caricamento Tabella Depot DPD
185100000908     c                   clear                   xx
185200000908     c                   clear                   DepK
185300091030     c                   clear                   DepK2
185400000908     c                   clear                   DepL
185500000908     c                   clear                   DepC
185600000908     c     'DEP'         chain     tntbe000                           31
185700000908      *
185800000908     c                   DOW       *in31 = *off
185900000908      * Se il S.I. è indicato deve essere uguale al mio
186000000908     c                   IF        TBEatb = *BLANKS  and
186100000908     c                              (TBEsif = Knsif  or  TBEsif = *blanks)
186200091106     C******                         and TBEke2='I              '
186300000908     c                   add       1             xx
186400000908     c                   movel     TBEke1        DepK(XX)
186500091106     C******
186600091106      ****  chiave completa  composta dalle 2 chiavi
186700091106     c                   movel     TBEke1        DepK2(XX)
186800091106     c                   move      TBEke2        DepK2(XX)
186900091106      ****
187000000908     c                   movel     TBEuni        DDEP
187100000908     c                   movel     §DEPlnp       DepL(XX)
187200000908     c                   movel     §DEPksc       DepC(XX)
187300051219     c                   movel     §DEPtar       DepT(XX)
187400150604     c                   movel     §DEPtarVTG    DepTvtg(XX)
187500051219     c                   movel     §DEPctm       DepM(XX)
187600051219     c                   movel     §DEPwrk       DepW(XX)
187700000908     c                   endif
187800000908     c     'DEP'         reade     tntbe000                               31
187900000908     c                   enddo
188000000908      *
188100971216      * Recupero data e ora
188200971216     C                   TIME                    WORA
188300991124     C                   TIME                    W0140
188400991124      * UDATE IN GGMMAAAA
188500991124     C                   MOVE      W0140         WDTGIO
188600991124      * UDATE IN AAAAMMGG
188700991124     C     *eur          MOVEL     WDTGIO        DATA_eur
188800991124     C     *iso          MOVEL     DATA_eur      dateu
188900141103     C                   MOVEL     DATA_eur      Data_limit_BLP
189000141103      *-------------
189100141103     C***
189200141103     C* CARICO TABELLA DPD per controlli in immissione bolle Partenza
189300141103     C                   clear                   DS3IDP
189400141103     C                   Z-ADD     1             TBLKUT
189500141103     C                   MOVEL     '3I'          TBLCOD
189600141103     C                   MOVEL(P)  'DPD'         TBLKEY
189700141103     C     KTABel        CHAIN     TABEL00F                           30
189800141103     C                   IF        %Found(TABEL00F) and tblflg = *blank
189900141103     C                   MOVEL     TBLUNI        DS3IDP
190000141103     c                   subdur    §3iGGU:*d     Data_limit_BLP
190100141103     c                   movel     Data_limit_BLPDATEUMENOGG
190200141103     C                   END
190300141103     C***
190400141103      *-------------
190500991124     C                   ENDSR
190600060621      * ?------------------------------------------------------------------ */
190700060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
190800060621      * ?------------------------------------------------------------------ */
190900060621     C     *pssr         BEGSR
191000060621     C
191100060710     C                   eval      esito = '2'
191200120504     C*
191300120504     C                   eval      wrkMsg='Il File DPD ricevuto è arrivato inc'+
191400120514     C                             'omlpeto. Controllare UPLOAD ($1) '+
191500120504     C                             'IGATEAVIS con stato 2: '+
191600120504     c                                  Depot_Trs_UNO + ' : ' +
191700120504     c                                   User_Trs_UNO + ' : ' +
191800120504     c                                   %editc(Data_Trs_UNO:'X') + ' : ' +
191900120504     c                                   %editc(Time_Trs_UNO:'X') + ' : ' +
192000120504     c                                   %editc(Num_Trs_UNO:'Z')
192100090605      **    avviso tramite MAIL
192200090605     C                   exSR      eMail_alert
192300090605      **
192400060621     C
192500060621     C                   ENDSR     '*CANCL'
192600060621     C
192700090605      * ?------------------------------------------------------------------ */
192800090605      *?  Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
192900090605      * ?------------------------------------------------------------------ */
193000090605     C     eMail_alert   BEGSR
193100090605     C*
193200090605     C                   eval      wrkEml =
193300090605     C                             'Andrea.Bertocchi@bartolini.it'
193400140828     C*
193500140828     C                   eval      wrkMsg =
193600140828     C                             Identifica_file
193700140828     c                             +':/N'+
193800140828     C                             Identifica_record
193900090605     C*
194000090605     C                   eval      wrkOgg='UPLOAD DPD IGATEAVIS NON completo -
194100090605     C                             in Stato 2.'
194200090605     C*
194300140828     C*******            call(e)   'TIS701C'
194400140828     C                   call(e)   'TRTCT00R2'
194500090605     C                   parm                    wrkEml          253
194600090605     C                   parm                    wrkOgg           44
194700090605     C                   parm                    wrkMsg         5000
194800090605      *
194900090605     C                   eval      wrkEml =
195000090605     C                             'CedAlert@bartolini.it'
195100090605     C*
195200140828     C*******            call(e)   'TIS701C'
195300140828     C                   call(e)   'TRTCT00R2'
195400090605     C                   parm                    wrkEml          253
195500090605     C                   parm                    wrkOgg           44
195600090605     C                   parm                    wrkMsg         5000
195700090605      *
195800090605     C                   ENDSR
195900060614      *-------------------------------------------------------------------------
