000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600060808      *  Importa da FTP server i Collection request DPD                         *
000700060808      *  TRASCODIFICA : CRDATA01      IMPORT DPD nuovo parcel e depot           *
000800060612      **?************************************************************************
000900060612     Ftivin00r  uF   E             DISK    usropn
001000060808      *---------------------------------------------------------------------*
001100080430     ffiDPO03l  if   E           K DISK    rename(FIDPO000:FIDPO003)
001200080430     f                                     prefix(DP3:3)
001300060925     fedtab01l  if   E           K DISK
001400060808     ftabel00f  if   E           K DISK
001500060915     ffnacr01l  if   E           K DISK
001600060808     fcnaco00f  if   E           K DISK
001700080512     FFnVAO00F  O  a E             DISK
001800060808     FFIdpo00F  O  a E             DISK
001900171106      *
002000171106     FdpCDP12i  IF   E           K DISK
002100171106      *
002200171106      ** ---------------------------------------------------------*
002300171106     D/COPY GAITRASRC/SRCPROTOPI,TISID1R
002400060808      *---------------------------------------------------------------------*
002500060808     D Ds15          e DS
002600060808     D  Naz03          s              3    DIM(999)
002700060808     D  Iso03          s              3    DIM(999)
002800060809     D  Cie03          s              3  0 DIM(999)
002900060808     D  Dpd03          s              3  0 DIM(999)
003000080604     D  POE03          s              3  0 DIM(999)
003100060808     D Dorm01        e DS
003200060925     D TRULDEPds     e DS
003300060925     D edidsPT       e DS
003400061006      *---------------------------------------------------------------------*
003500061006     D* DS PER FNLV13R - CONTROLLO DI UN CAP
003600061006     D DSLV13        E DS                  EXTNAME(FNLV13DS)
003700061006     D* DS PER TISI95R - CONTROLLO DI CAP
003800061006     D DSSI95        E DS                  EXTNAME(TISI95DS)
003900061006     D kpjba         e DS
004000060808      * ?================================================================== */
004100060612     D Lung_Parcel     S              3  0
004200060620     D Parcel11        S             11
004300060710     D Parcel14        S             14
004400060809     D Tipo_error      S              1  0
004500060808      * ?================================================================== */
004600060808     D  data_Oggi      s              8  0 INZ(0)
004700060808     D  day            s              8  0 INZ(0)
004800060808     D  ore            s              6  0 INZ(0)
004900060808     D  w0140          s             14  0 INZ(0)
005000060809      *
005100060808     D WLBDAT          DS
005200060808     D  G02DAT                 1      8  0
005300060808     D  G02INV                 9     16  0
005400060808     D  G02ERR                17     17
005500060808     D  G02TGI                18     22  0
005600060809      * ?   *--------------------------------------------------------------*
005700060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
005800060809      * ?   *--------------------------------------------------------------*
005900060809     D FIDPODTA1     e DS
006000061002     d  ONAME_maiu     s                   like(ONAME)
006100061002     d  ONAME2_maiu    s                   like(ONAME2)
006200061002     d  OSTREET_maiu   s                   like(OSTREET)
006300061002     d  OCITY_maiu     s                   like(OCITY)
006400061002     d  CNAME_maiu     s                   like(CNAME)
006500061002     d  CNAME2_maiu    s                   like(CNAME2)
006600061002     d  CNAME3_maiu    s                   like(CNAME3)
006700061002     d  CNAME4_maiu    s                   like(CNAME4)
006800061002     d  CSTREET_maiu   s                   like(CSTREET)
006900061002     d  CCITY_maiu     s                   like(CCITY)
007000061002     d  RNAME_maiu     s                   like(RNAME)
007100061002     d  RNAME2_maiu    s                   like(RNAME2)
007200061002     d  RSTREET_maiu   s                   like(RSTREET)
007300061002     d  RCITY_maiu     s                   like(RCITY)
007400061002     d  CUSTREF_maiu   s                   like(CUSTREF)
007500061002     d  INFO1_maiu     s                   like(INFO1)
007600061002     d  INFO2_maiu     s                   like(INFO2)
007700060612      * ?================================================================== */
007800060809      * ?   * Campi da tradurre con interi
007900060809     D atoll           PR            20I 0
008000060809     D                                     EXTPROC('atoll')
008100060809     D  Char                           *
008200060809     D                                     VALUE
008300060809     D                                     OPTIONS(*STRING)
008400060612      * ?   * Campi da tradurre con interi
008500060612     D atoi            PR            10I 0
008600060612     D                                     EXTPROC('atoi')
008700060612     D  Char                           *
008800060612     D                                     VALUE
008900060612     D                                     OPTIONS(*STRING)
009000060612      * ?   * Campi da tradurre con decimali
009100060612     D atof            PR             8F
009200060612     D                                     EXTPROC('atof')
009300060612     D  Char                           *
009400060612     D                                     VALUE
009500060612     D                                     OPTIONS(*STRING)
009600060612      * ?   *--------------------------------------------------------------*
009700060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
009800060612      * ?   *--------------------------------------------------------------*
009900060612      *
010000060612      * ?   * Campi x decodifica * (INPUT  del Record)
010100060612     D  Dati           s           1500
010200060612     D Separa_campi    s              1
010300060612     D Decimal_Separ   s              1
010400060809      *
010500060612      * ?   * Campi decodificati * (OUTPUT del Record)
010600060612      *       Schiere di Output / Campi di Output
010700060614     D  Alfa40         s             40
010800060612     d  Sk             s              3u 0
010900060612     D    NR_campi     s              3u 0
011000060612     D  Campi_Record   ds
011100060612     D    Dato_sk                    40    DIM(100)
011200060612     D  Campi_Lunghi   ds
011300060612     D    LunDato_sk                  3u 0 DIM(100)
011400060612     D Campi_Numerici  ds
011500060612     D    DatoNum_sk                  1    DIM(100)
011600060612     D Campi_Decimali  ds
011700060612     D    Decimal_sk                  3u 0 DIM(100)
011800060612      * ?   *--------------------------------------------------------------*
011900060612     D  position       s              3  0 INZ(0)
012000060612
012100060612     D  se_errore      s              1    inz(' ')
012200060612     D  Msg_Err        s            132    inz(' ')
012300060614     D  Msg_Vin_80     s             80    inz(' ')
012400060612      * ?* ------------------------------------------------------ *
012500060612      * Records di controllo mandati nell'IGATEAVIS
012600060612      * ?   * Dati Record UNO *
012700060612     DDepot_Trs_UNO    s             10
012800060612     D User_Trs_UNO    s             10
012900060612     D Data_Trs_UNO    s              8  0
013000060612     D Time_Trs_UNO    s              6  0
013100060612     D  Num_Trs_UNO    s              9  0
013200060612      * ?   * Dati Record DUE *
013300060809     D Vers_Trs_DUE    s             15
013400060809      * ?   * Dati Record TRE *
013500060809     D Testa_Sk        s             10    DIM(50)
013600060809      * ?   * Dati Record QUATTRO *
013700060612     D  Num_Trs_FIN    s              9  0
013800060710      **
013900061002     d minu            c                   const('qwertyuiopasdfghjklzxcvbnmàáâ-*alfabeto
014000061002     d                                     ãäåèéêëìíîïòóôõöùúûüæçðñßÑÀÁÂÃÄÅÆÈÉÊ-
014100061002     d                                     ËÌÍÎÏÒÓÔÕÖÙÚÛÜ')
014200061002     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMAAA-*ALFABETO
014300061002     d                                     AAAEEEEIIIIOOOOOUUUUACANSNAAAAAAAEEE-
014400061002     d                                     EIIIIOOOOOUUUU')
014500061002
014600060710     D Digits          C                   '0123456789'
014700060612      * ?================================================================== */
014800060612      *   Ciclo principale
014900060413      * ?********************************************************************/
015000060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
015100060612     C*
015200060612     C                   if        not %open(tivin00r)
015300060612     C                   open      tivin00r
015400060612     C                   endif
015500060612      **
015600060612      * ?------------------------------------------------------------------ */
015700060612     C*? Controllo dati arrivati da DPD
015800060612      * ?------------------------------------------------------------------ */
015900060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
016000060612      * ?- controllando che la trasmissione sia completa.
016100060612
016200060612      * ?              /*---------------------- */
016300060612     c                   exsr      check_Trasm
016400060612      * ?              /*---------------------- */
016500060612      **
016600060613      **  Errore di trasmissione x tutti i records
016700060613      **   --> file in errore
016800060613     c                   if        se_errore = 'S'
016900060612
017000060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
017100060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
017200060613     C                               >> Farsi rimandare i DATI !!'
017300060612
017400060612      * ?  Scrive su tutti i records il tipo di errore
017500060612     c     *start        setll     tivin00r
017600060612     c                   read      tivin00r
017700060612     c                   dow       not %eof(tivin00r)
017800060612     C                   eval      vinMSG = Msg_Err
017900060612     C                   eval      vinFLG = '2'
018000060613     c                   eval      esito  = '2'
018100060612     c                   update    tivin000
018200060612     c                   read      tivin00r
018300060612     c                   endDO
018400060612      **
018500060612     c                   else
018600060614      **
018700060612      * ?------------------------------------------------------------------ */
018800060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
018900060612      * ?------------------------------------------------------------------ */
019000060612      * ?              /*---------------------- */
019100060612     c                   exsr      Importa_Msg
019200060612      * ?              /*---------------------- */
019300060612
019400060614     c                   end
019500060614
019600060612     C                   if        %open(tivin00r)
019700060612     C                   close     tivin00r
019800060612     C                   endif
019900060612      *
020000060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
020100060614      *
020200060710     c                   if        almeno_uno ='S' and esito ='1'
020300060710     C                   eval      esito ='0'
020400060614     C                   endif
020500060614      *
020600060612     c                   SETON                                        LR
020700060612      * ?================================================================== */
020800060612      *? Controlla la trasmissione   se completa
020900060612      * ?------------------------------------------------------------------ */
021000060612     c     Check_Trasm   Begsr
021100060612
021200060612     C                   clear                   se_errore
021300060612      ** primo  record
021400060612     c     *start        setll     tivin00r
021500060612     c                   read      tivin00r
021600060612     c                   if        not %eof(tivin00r)
021700060612      * ?              /*---------------------- */
021800060612     c                   exsr      Decod_Record
021900060612      * ?              /*---------------------- */
022000060612     c                   endif
022100060612      **
022200060612      ** ultimo record
022300060612     c     *hival        setll     tivin00r
022400060612     c                   readp     tivin00r
022500060612     c                   if        not %eof(tivin00r)
022600060612      * ?              /*---------------------- */
022700060612     c                   exsr      Decod_Record
022800060612      * ?              /*---------------------- */
022900060612     c                   endif
023000060612
023100060612      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
023200060612      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
023300060612      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
023400060612     C                   if        num_trs_UNO <> num_trs_FIN
023500060612      * ?-----> Errore
023600060612     C                   eval      se_errore = 'S'
023700060612
023800060612     c                   end
023900060612
024000060612     c                   endSR
024100060612      * ?------------------------------------------------------------------ */
024200060612      *? Importa i records della tramsissione
024300060612      * ?------------------------------------------------------------------ */
024400060612     c     Importa_Msg   Begsr
024500060612
024600060614      * per controllare se almeno un record è stato importato sul VAB
024700060614     c                   clear                   Almeno_Uno        1
024800060614
024900060612     c     *start        setll     Tivin00r
025000060612     c                   read      Tivin00r
025100060612
025200060612     c                   dow       not %eof(Tivin00r)
025300060614
025400060614      * solo i record sflaggati da rielaborare
025500060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
025600060612
025700060612      ** Controlli formali sui campi
025800060612     c                   clear                   se_errore
025900060620     C                   clear                   Msg_Vin_80
026000060612
026100060612      ** Decodifica record a campi variabili
026200060612      * ?              /*---------------------- */
026300060612     c                   exsr      Decod_Record
026400060612      * ?              /*---------------------- */
026500060612
026600060612      **  Se presente un errore nel record emette una segnalazione msg
026700060612     c                   if        se_errore ='S'
026800060614     C                   eval      vinMSG = Msg_Vin_80
026900060621     c                   end
027000060612
027100060809      *  con il record in canna scrive direttamente il VAO
027200060620     c                   if        %subst(dati:1:1) <> '#'
027300060612      * ?              /*---------------------- */
027400060809     c                   exsr      WRT_FILE
027500060612      * ?              /*---------------------- */
027600060620     c                   end
027700060614
027800060809      *  Problemi nella decodifica dei campi VAO
027900060614     c                   if        se_errore ='S'
028000060614     C                   eval      vinMSG = Msg_Vin_80
028100060614     c                   end
028200060612
028300060621      *  Sempre Record OK
028400060621     C                   eval      vinFLG = '1'
028500060710     c                   eval      esito  = '0'
028600060621
028700060809      *  x errore bloccante nella composizione del VAO
028800060621     c                   if        err_bloccante ='S'
028900060621     C                   eval      vinFLG = '2'
029000060710     c                   eval      esito  = '1'
029100060621     c                   end
029200060612
029300060612     c                   update    Tivin000
029400060614     c                   endIF
029500060612
029600060612     c                   read      Tivin00r
029700060612     C                   ENDdo
029800060612      **
029900060612     c                   endSR
030000060612      * ?------------------------------------------------------------------ */
030100060612      *?    Decodifica record a campi variabili
030200060612      * ?------------------------------------------------------------------ */
030300060612     c     Decod_Record  Begsr
030400060612
030500060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
030600060612     c                   movel(p)  VINDTA        dati
030700060612      **
030800060612      * ?              /*---------------------- */
030900060612     c                   exsr      Split_Rec
031000060612      * ?              /*---------------------- */
031100060612      **
031200060612      **  Records di Intestazione e di fine Msg
031300060612     c                   if        %subst(dati:1:1) ='#'
031400060612      * ?              /*---------------------- */
031500060612     c                   exsr      Rec_Speciali
031600060612      * ?              /*---------------------- */
031700060612     c                   Else
031800060612      ** conta le righe
031900060612      * ?              /*---------------------- */
032000060612     c                   exsr      Rec_Dettaglio
032100060612      * ?              /*---------------------- */
032200060612     c                   End
032300060612      **
032400060612     c                   Endsr
032500060612      * ?------------------------------------------------------------------ */
032600060612      *?      Record di Testata e di Fine Msg
032700060612      * ?------------------------------------------------------------------ */
032800060612     c     Rec_Speciali  Begsr
032900060612      **
033000060612      * ?  Prima riga
033100060612     c     'FILE'        SCAN      dati:2        position
033200060612     c                   if        %Found and position < 5
033300060612     c                   exsr      Rec_UNO
033400060612     c                   ElSe
033500060612     c                   End
033600060612      **
033700060809      * ?  Seconda riga (verisone)
033800060809     c     'Version'     SCAN      dati:2        position
033900060612     c                   if        %Found and position < 5
034000060809     c                   exsr      Rec_DUE
034100060612     c                   End
034200060809      **
034300060809      * ?  Terza riga (testate dei campi)
034400060809     c     'DEF'         SCAN      dati:2        position
034500060809     c                   if        %Found and position < 5
034600060809     c                   exsr      Rec_TRE
034700060809     c                   End
034800060612      **
034900060612      * ?  Ultima riga
035000060612     c     'END'         SCAN      dati:2        position
035100060612     c                   if        %Found and position < 5
035200060612     c                   exsr      Rec_LAST
035300060612     c                   End
035400060612      **
035500060612     c                   Endsr
035600060612      * ?------------------------------------------------------------------ */
035700060612      *?      Primo Record
035800060612      * ?------------------------------------------------------------------ */
035900060612     c     Rec_UNO       Begsr
036000060612      **
036100060612      ** esempio di record :
036200060808      * ?   # FILE;N00;depot945;20050603;143526;6;
036300060612      **
036400060612      **  Inizializza i campi della trasmissione
036500060612     C                   eval      Depot_Trs_UNO = *blank
036600060612     C                   eval      User_Trs_UNO  = *blank
036700060612     C                   eval      Data_Trs_UNO  = 0
036800060612     C                   eval      Time_Trs_UNO  = 0
036900060612     C                   eval      Num_Trs_UNO   = 0
037000060612
037100060612      **   Memorizza il numero della trasm. da confrontare con l'ultimo record.
037200060612      **     Tutto il dettaglio deve essere compreso entro questi 2 records
037300060612      **    altrimenti il msg.non è completo e bisogna farsi rimandare il file.
037400060612
037500060808      * ? Nel 2° campo di testata c'è lo USER in Delis
037600060808     C                   z-add     2             sk
037700060808     c                   eval      User_Trs_UNO = %subst(Dato_sk(sk):1:+
037800060808     c                             LunDato_sk(sk))
037900060808
038000060808      * ? Nel 3° campo di testata c'è il Dispatch Depot
038100060808     C                   z-add     3             sk
038200060808     c                   eval      Depot_Trs_UNO = %subst(Dato_sk(sk):1:+
038300060808     c                             LunDato_sk(sk))
038400060808
038500060808      * ? Nel 4° campo di testata c'è la data trasmissione
038600060808     C                   z-add     4             sk
038700060808     c                   if        DatoNum_sk(sk) = 'S'
038800060808     c                   eval      Data_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
038900060808     c                   End
039000060808
039100060808      * ? Nel 5° campo di testata c'è il nr.seriale di trasmissione
039200060808     C                   z-add     5             sk
039300060808     c                   if        DatoNum_sk(sk) = 'S'
039400060808     c                   eval      Time_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
039500060808     c                   End
039600060808
039700060808      * ? Nel 6° campo di testata c'è il nr.seriale di trasmissione
039800060808     C                   z-add     6             sk
039900060808     c                   if        DatoNum_sk(sk) = 'S'
040000060808     c                   eval      num_trs_UNO = atoi(%TRIM(Dato_sk(sk)))
040100060808     c                   End
040200060612      **
040300060612     c                   Endsr
040400060612      * ?------------------------------------------------------------------ */
040500060612      *?      Secondo Record
040600060612      * ?------------------------------------------------------------------ */
040700060612     c     Rec_DUE       Begsr
040800060809      **
040900060809      ** esempio di record :
041000060809      * ?   # Version;JR20040831;
041100060809      **
041200060809      **  Inizializza i campi della versione trasmissione
041300060809     C                   eval      Vers_Trs_DUE = *blank
041400060809      **
041500060809      **   Memorizza la versione i trasm.
041600060809
041700060809      * ? Nel 2° campo di testata c'è lo USER in Delis
041800060809     C                   z-add     2             sk
041900060809     c                   eval      Vers_Trs_DUE = %subst(Dato_sk(sk):1:+
042000060809     c                             LunDato_sk(sk))
042100060809      **
042200060809     c                   Endsr
042300060809      * ?------------------------------------------------------------------ */
042400060809      *?      Terzo   Record
042500060809      * ?------------------------------------------------------------------ */
042600060809     c     Rec_TRE       Begsr
042700060809      **
042800060612      ** esempio di record :
042900060808      * ?   # DEF;CR:CRDATA01;ODEPOT;ORDERNR;CDEPOT;OCUSTNO;ONAME;ONAME2;OS...
043000060612      **
043100060612      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
043200060612     C                   clear                   Testa_Sk
043300060612
043400060612      * ? Dal 3° campo troviamo i nomi delle intestazioni dei campi C.Request
043500060612     C     3             do        50            sk
043600060612     c                   eval      Testa_Sk(sk-2) = %subst(Dato_sk(sk):1:+
043700060612     c                             LunDato_sk(sk))
043800060612     c                   enddo
043900060612      **
044000060612     c                   Endsr
044100060612      * ?------------------------------------------------------------------ */
044200060612      *?      Ultimo  Record
044300060612      * ?------------------------------------------------------------------ */
044400060612     c     Rec_LAST      Begsr
044500060612      **
044600060612      ** esempio di record :
044700060808      * ?   # END;6;
044800060612      **
044900060612      **  Inizializza i campi della trasmissione
045000060612     C                   eval      Num_Trs_fin = 0
045100060612      **
045200060612      **  Nell'ultimo record di chiusura deve esserci lo stesso numero di trasmissione
045300060612      **  impostato nel primo record.
045400060612      **
045500060612      * ? Nel 2° campo di fine trsm. c'è il nr.seriale di trasmissione
045600060612     C                   z-add     2             sk
045700060612     c                   if        DatoNum_sk(sk) = 'S'
045800060612     c                   eval      num_trs_fin = atoi(%TRIM(Dato_sk(sk)))
045900060612     c                   End
046000060612      **
046100060612     c                   Endsr
046200060612      * ?------------------------------------------------------------------ */
046300060612      *?      Riga Bolla Import da controllare e tradurre
046400060612      * ?------------------------------------------------------------------ */
046500060612     c     Rec_Dettaglio Begsr
046600060612      **
046700060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
046800060612      **
046900060809     c                   clear                   fidpoDTA1
047000060614     c                   clear                   Msg_Err
047100110930     c                   clear                   ONAME_maiu
047200110930     c                   clear                   ONAME2_maiu
047300110930     c                   clear                   OSTREET_maiu
047400110930     c                   clear                   OCITY_maiu
047500110930     c                   clear                   CNAME_maiu
047600110930     c                   clear                   CNAME2_maiu
047700110930     c                   clear                   CNAME3_maiu
047800110930     c                   clear                   CNAME4_maiu
047900110930     c                   clear                   CSTREET_maiu
048000110930     c                   clear                   CCITY_maiu
048100110930     c                   clear                   RNAME_maiu
048200110930     c                   clear                   RNAME2_maiu
048300110930     c                   clear                   RSTREET_maiu
048400110930     c                   clear                   RCITY_maiu
048500110930     c                   clear                   CUSTREF_maiu
048600110930     c                   clear                   INFO1_maiu
048700110930     c                   clear                   INFO2_maiu
048800060808      **
048900060808      * ? Nel 1° campo : Numero Depot Ordinante DPD  (Oblligatorio)  ODEPOT
049000060808      **   parte 1 della chiave ORM DPD                              DEPOT - ORDINANTE
049100060808     C                   z-add     1             sk
049200060809     c                   If        LunDato_sk(sk) =  0
049300060808      * ?-----> Errore
049400060809     C                   eval      tipo_error = 1
049500060809     C                   exsr      Field_Error
049600060808     c                   Else
049700060809     c                   eval      ODEPOT   = %subst(Dato_sk(sk):1:+
049800060809     c                                              LunDato_sk(sk))
049900060808     c                   End
050000060808      **
050100060808      * ? Nel 2° campo : Numero Ordine DPD           (Oblligatorio)  ORDERNR
050200060808      **   parte 2 della chiave ORM DPD                              NUM.ORDINE
050300060808     C                   add       1             sk
050400060808     c                   If        DatoNum_sk(sk) = 'N' or
050500060808     c                             LunDato_sk(sk) =  0
050600060808      * ?-----> Errore
050700060809     C                   eval      tipo_error = 2
050800060809     C                   exsr      Field_Error
050900060808     c                   Else
051000060808     c                   eval      ORDERNR  = atoi(%TRIM(Dato_sk(sk)))
051100060808     c                   End
051200060808      **
051300060808      * ? Nel 3° campo : Depot Che deve eseguire ORM                 CDEPOT
051400060808      **                                                             DEPOT DI RITIRO
051500060808     C                   add       1             sk
051600060809     c                   If        LunDato_sk(sk) =  0
051700060808      * ?-----> Errore
051800060809     C                   eval      tipo_error = 1
051900060809     C                   exsr      Field_Error
052000060808     c                   Else
052100060809     c                   eval      CDEPOT   = %subst(Dato_sk(sk):1:+
052200060809     c                                              LunDato_sk(sk))
052300060808     c                   End
052400060808      **
052500060808      * ? Nel 4° campo : Codice Cliente Ordinante                    OCUSTNO
052600060808      **                                                             CLIENTE ORDINANTE
052700060808     C                   add       1             sk
052800060809     c                   If        DatoNum_sk(sk) = 'N' or
052900060809     c                             LunDato_sk(sk) =  0
053000060808      * ?-----> Errore
053100060809     C                   eval      tipo_error = 2
053200060809     C                   exsr      Field_Error
053300060808     c                   Else
053400060808     c                   eval      OCUSTNO  = atoi(%TRIM(Dato_sk(sk)))
053500060808     c                   End
053600060808      **
053700060808      * ? Nel 5° campo : Nome Cliente Ordinante      (Oblligatorio)  ONAME
053800060808      **                                                             NOME ORDINANTE
053900060808     C                   add       1             sk
054000060808     c                   If        LunDato_sk(sk) =  0
054100060808      * ?-----> Errore
054200060809     C                   eval      tipo_error = 1
054300060809     C                   exsr      Field_Error
054400060808     c                   Else
054500060808     c                   eval      ONAME    = %subst(Dato_sk(sk):1:+
054600060808     c                                              LunDato_sk(sk))
054700061002     c     minu:maiu     XLATE     ONAME         ONAME_maiu                     *Minus -> Maiuscolo
054800060808     c                   End
054900060808      **
055000060808      * ? Nel 6° campo : 2° Nome Cliente Ordinante                   ONAME2
055100060808      **                                                             NOME 2 ORDINANTE
055200060808     C                   add       1             sk
055300060808     c                   If        LunDato_sk(sk) >  0
055400060808     c                   eval      ONAME2   = %subst(Dato_sk(sk):1:+
055500060808     c                                              LunDato_sk(sk))
055600061002     c     minu:maiu     XLATE     ONAME2        ONAME2_maiu                    *Minus -> Maiuscolo
055700060808     c                   End
055800060808      **
055900060808      * ? Nel 7° campo : Indirizzo Ordinante         (Oblligatorio)  OSTREET
056000060808      **                                                             INDIRIZZO ORDINANTE
056100060808     C                   add       1             sk
056200060808     c                   If        LunDato_sk(sk) =  0
056300060808      * ?-----> Errore
056400060809     C                   eval      tipo_error = 1
056500060809     C                   exsr      Field_Error
056600060808     c                   Else
056700060808     c                   eval      OSTREET  = %subst(Dato_sk(sk):1:+
056800060808     c                                              LunDato_sk(sk))
056900061002     c     minu:maiu     XLATE     OSTREET       OSTREET_maiu                   *Minus -> Maiuscolo
057000060808     c                   End
057100060808      **
057200060808      * ? Nel 8° campo : Nazione Ordinante           (Oblligatorio)  OCOUNTRY
057300060808      **                                                             NAZIONE ORDINANTE
057400060808     C                   add       1             sk
057500060809     c                   If        DatoNum_sk(sk) = 'N' or
057600060809     c                             LunDato_sk(sk) =  0
057700060809      * ?-----> Errore
057800060809     C                   eval      tipo_error = 2
057900060809     C                   exsr      Field_Error
058000060808     c                   Else
058100060809     c                   eval      OCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
058200060808     c                   End
058300060808      **
058400060808      * ? Nel 9° campo : Cod.Postale Ordinante       (Oblligatorio)  OPOSTAL
058500060808      **                                                             CAP ORDINANTE
058600060808     C                   add       1             sk
058700060808     c                   If        LunDato_sk(sk) =  0
058800060808      * ?-----> Errore
058900060809     C                   eval      tipo_error = 1
059000060809     C                   exsr      Field_Error
059100060808     c                   Else
059200060808     c                   eval      OPOSTAL  = %subst(Dato_sk(sk):1:+
059300060808     c                                              LunDato_sk(sk))
059400060808     c                   End
059500060808      **
059600060808      * ? Nel 10° campo : Città Ordinante            (Oblligatorio)  OCITY
059700060808      **                                                             CITTÀ ORDINANTE
059800060808     C                   add       1             sk
059900060808     c                   If        LunDato_sk(sk) =  0
060000060808      * ?-----> Errore
060100060809     C                   eval      tipo_error = 1
060200060809     C                   exsr      Field_Error
060300060808     c                   Else
060400060808     c                   eval      OCITY    = %subst(Dato_sk(sk):1:+
060500060808     c                                              LunDato_sk(sk))
060600061002     c     minu:maiu     XLATE     OCITY         OCITY_maiu                     *Minus -> Maiuscolo
060700060808     c                   End
060800060808      **
060900060808      * ? Nel 11° campo : Telefono Ordinante                         OPHONE
061000060808      **                                                             TELEF ORDINANTE
061100060808     C                   add       1             sk
061200060808     c                   If        LunDato_sk(sk) >  0
061300060808     c                   eval      OPHONE   = %subst(Dato_sk(sk):1:+
061400060808     c                                              LunDato_sk(sk))
061500060808     c                   End
061600060808      **
061700060808      * ? Nel 12° campo : E-mail   Ordinante                         OEMAIL
061800060808      **                                                             EMAIL ORDINANTE
061900060808     C                   add       1             sk
062000060808     c                   If        LunDato_sk(sk) >  0
062100060808     c                   eval      OEMAIL   = %subst(Dato_sk(sk):1:+
062200060808     c                                              LunDato_sk(sk))
062300060808     c                   End
062400060808      **
062500060808      * ? Nel 13° campo : Nome di Ritiro             (Oblligatorio)  CNAME
062600060808      **                                                             NOME DI RITIRO
062700060808     C                   add       1             sk
062800060808     c                   If        LunDato_sk(sk) =  0
062900060808      * ?-----> Errore
063000060809     C                   eval      tipo_error = 1
063100060809     C                   exsr      Field_Error
063200060808     c                   Else
063300060808     c                   eval      CNAME    = %subst(Dato_sk(sk):1:+
063400060808     c                                              LunDato_sk(sk))
063500061002     c     minu:maiu     XLATE     CNAME         CNAME_maiu                     *Minus -> Maiuscolo
063600060808     c                   End
063700060808      **
063800060808      * ? Nel 14° campo : 2° Nome di Ritiro                          CNAME2
063900060808      **                                                             NOME 2 DI RITIRO
064000060808     C                   add       1             sk
064100060808     c                   If        LunDato_sk(sk) >  0
064200060808     c                   eval      CNAME2   = %subst(Dato_sk(sk):1:+
064300060808     c                                              LunDato_sk(sk))
064400061002     c     minu:maiu     XLATE     CNAME2        CNAME2_maiu                    *Minus -> Maiuscolo
064500060808     c                   End
064600060808      **
064700060808      * ? Nel 15° campo : 3° Nome di Ritiro                          CNAME3
064800060808      **                                                             NOME 3 DI RITIRO
064900060808     C                   add       1             sk
065000060808     c                   If        LunDato_sk(sk) >  0
065100060808     c                   eval      CNAME3   = %subst(Dato_sk(sk):1:+
065200060808     c                                              LunDato_sk(sk))
065300061002     c     minu:maiu     XLATE     CNAME3        CNAME3_maiu                    *Minus -> Maiuscolo
065400060808     c                   End
065500060808      **
065600060808      * ? Nel 16° campo : 4° Nome di Ritiro                          CNAME4
065700060808      **                                                             NOME 4 DI RITIRO
065800060808     C                   add       1             sk
065900060808     c                   If        LunDato_sk(sk) >  0
066000060808     c                   eval      CNAME4   = %subst(Dato_sk(sk):1:+
066100060808     c                                              LunDato_sk(sk))
066200061002     c     minu:maiu     XLATE     CNAME4        CNAME4_maiu                    *Minus -> Maiuscolo
066300060808     c                   End
066400060808      **
066500060808      * ? Nel 17° campo : Indirizzo di Ritiro        (Oblligatorio)  CSTREET
066600060808      **                                                             INDIRIZZO DI RITIRO
066700060808     C                   add       1             sk
066800060808     c                   If        LunDato_sk(sk) =  0
066900060808      * ?-----> Errore
067000060809     C                   eval      tipo_error = 1
067100060809     C                   exsr      Field_Error
067200060808     c                   Else
067300060808     c                   eval      CSTREET  = %subst(Dato_sk(sk):1:+
067400060808     c                                              LunDato_sk(sk))
067500061002     c     minu:maiu     XLATE     CSTREET       CSTREET_maiu                   *Minus -> Maiuscolo
067600060808     c                   End
067700060808      **
067800060808      * ? Nel 18° campo : Nazione di Ritiro          (Oblligatorio)  CCOUNTRY
067900060808      **                                                             NAZIONE DI RITIRO
068000060808     C                   add       1             sk
068100060809     c                   If        DatoNum_sk(sk) = 'N' or
068200060809     c                             LunDato_sk(sk) =  0
068300060808      * ?-----> Errore
068400060809     C                   eval      tipo_error = 2
068500060809     C                   exsr      Field_Error
068600060808     c                   Else
068700060809     c                   eval      CCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
068800060808     c                   End
068900060808      **
069000060808      * ? Nel 19° campo : Cod.Postale di Ritiro      (Oblligatorio)  CPOSTAL
069100060808      **                                                             CAP DI RITIRO
069200060808     C                   add       1             sk
069300060808     c                   If        LunDato_sk(sk) =  0
069400060808      * ?-----> Errore
069500060809     C                   eval      tipo_error = 1
069600060809     C                   exsr      Field_Error
069700060808     c                   Else
069800060808     c                   eval      CPOSTAL  = %subst(Dato_sk(sk):1:+
069900060808     c                                              LunDato_sk(sk))
070000060808     c                   End
070100060808      **
070200060808      * ? Nel 20° campo : Città di Ritiro            (Oblligatorio)  CCITY
070300060808      **                                                             CITTÀ DI RITIRO
070400060808     C                   add       1             sk
070500060808     c                   If        LunDato_sk(sk) =  0
070600060808      * ?-----> Errore
070700060809     C                   eval      tipo_error = 1
070800060809     C                   exsr      Field_Error
070900060808     c                   Else
071000060808     c                   eval      CCITY    = %subst(Dato_sk(sk):1:+
071100060808     c                                              LunDato_sk(sk))
071200061002     c     minu:maiu     XLATE     CCITY         CCITY_maiu                     *Minus -> Maiuscolo
071300060808     c                   End
071400060808      **
071500060808      * ? Nel 21° campo : Telefono di Ritiro                         CPHONE
071600060808      **                                                             TELEF DI RITIRO
071700060808     C                   add       1             sk
071800060808     c                   If        LunDato_sk(sk) >  0
071900060808     c                   eval      CPHONE   = %subst(Dato_sk(sk):1:+
072000060808     c                                              LunDato_sk(sk))
072100060808     c                   End
072200060808      **
072300060808      * ? Nel 22° campo : E-mail   di Ritiro                         CEMAIL
072400060808      **                                                             EMAIL ORDINANTE
072500060808     C                   add       1             sk
072600060808     c                   If        LunDato_sk(sk) >  0
072700060808     c                   eval      CEMAIL   = %subst(Dato_sk(sk):1:+
072800060808     c                                              LunDato_sk(sk))
072900060808     c                   End
073000060808      **
073100060808      * ? Nel 23° campo : Nome di Destino            (Oblligatorio)  RNAME
073200060808      **                                                             NOME DI DESTINO
073300060808     C                   add       1             sk
073400060808     c                   If        LunDato_sk(sk) =  0
073500060808      * ?-----> Errore
073600060809     C                   eval      tipo_error = 1
073700060809     C                   exsr      Field_Error
073800060808     c                   Else
073900060808     c                   eval      RNAME    = %subst(Dato_sk(sk):1:+
074000060808     c                                              LunDato_sk(sk))
074100061002     c     minu:maiu     XLATE     RNAME         RNAME_maiu                     *Minus -> Maiuscolo
074200060808     c                   End
074300060808      **
074400060808      * ? Nel 24° campo : 2° Nome di Destino                         RNAME2
074500060808      **                                                             NOME 2 DI DESTINO
074600060808     C                   add       1             sk
074700060808     c                   If        LunDato_sk(sk) >  0
074800060808     c                   eval      RNAME2   = %subst(Dato_sk(sk):1:+
074900060808     c                                              LunDato_sk(sk))
075000061002     c     minu:maiu     XLATE     RNAME2        RNAME2_maiu                    *Minus -> Maiuscolo
075100060808     c                   End
075200060808      **
075300060808      * ? Nel 25° campo : Indirizzo di Destino       (Oblligatorio)  RSTREET
075400060808      **                                                             INDIRIZZO DI DESTINO
075500060808     C                   add       1             sk
075600060808     c                   If        LunDato_sk(sk) =  0
075700060808      * ?-----> Errore
075800060809     C                   eval      tipo_error = 1
075900060809     C                   exsr      Field_Error
076000060808     c                   Else
076100060808     c                   eval      RSTREET  = %subst(Dato_sk(sk):1:+
076200060808     c                                              LunDato_sk(sk))
076300061002     c     minu:maiu     XLATE     RSTREET       RSTREET_maiu                   *Minus -> Maiuscolo
076400060808     c                   End
076500060808      **
076600060808      * ? Nel 26° campo : Nazione di Destino         (Oblligatorio)  RCOUNTRY
076700060808      **                                                             NAZIONE DI DESTINO
076800060808     C                   add       1             sk
076900060809     c                   If        DatoNum_sk(sk) = 'N' or
077000060809     c                             LunDato_sk(sk) =  0
077100060808      * ?-----> Errore
077200060809     C                   eval      tipo_error = 2
077300060809     C                   exsr      Field_Error
077400060808     c                   Else
077500060809     c                   eval      RCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
077600060808     c                   End
077700060808      **
077800060808      * ? Nel 27° campo : Cod.Postale di Destino     (Oblligatorio)  RPOSTAL
077900060808      **                                                             CAP DI DESTINO
078000060808     C                   add       1             sk
078100060808     c                   If        LunDato_sk(sk) =  0
078200060808      * ?-----> Errore
078300060809     C                   eval      tipo_error = 1
078400060809     C                   exsr      Field_Error
078500060808     c                   Else
078600060808     c                   eval      RPOSTAL  = %subst(Dato_sk(sk):1:+
078700060808     c                                              LunDato_sk(sk))
078800060808     c                   End
078900060808      **
079000060808      * ? Nel 28° campo : Città di Destino           (Oblligatorio)  RCITY
079100060808      **                                                             CITTÀ DI DESTINO
079200060808     C                   add       1             sk
079300060808     c                   If        LunDato_sk(sk) =  0
079400060808      * ?-----> Errore
079500060809     C                   eval      tipo_error = 1
079600060809     C                   exsr      Field_Error
079700060808     c                   Else
079800060808     c                   eval      RCITY    = %subst(Dato_sk(sk):1:+
079900060808     c                                              LunDato_sk(sk))
080000061002     c     minu:maiu     XLATE     RCITY         RCITY_maiu                     *Minus -> Maiuscolo
080100060808     c                   End
080200060808      **
080300060808      * ? Nel 29° campo : Telefono di Destino                        RPHONE
080400060808      **                                                             TELEF DI DESTINO
080500060808     C                   add       1             sk
080600060808     c                   If        LunDato_sk(sk) >  0
080700060808     c                   eval      RPHONE   = %subst(Dato_sk(sk):1:+
080800060808     c                                              LunDato_sk(sk))
080900060808     c                   End
081000060808      **
081100060808      * ? Nel 30° campo : E-mail   di Destino                        REMAIL
081200060808      **                                                             EMAIL DI DESTINO
081300060808     C                   add       1             sk
081400060808     c                   If        LunDato_sk(sk) >  0
081500060808     c                   eval      REMAIL   = %subst(Dato_sk(sk):1:+
081600060808     c                                              LunDato_sk(sk))
081700060808     c                   End
081800060808      **
081900060808      * ? Nel 31° campo : Depot di Destino                           RDEPOT
082000060808      **                                                             DEPOT DI DESTINO
082100060808     C                   add       1             sk
082200060809     c                   If        LunDato_sk(sk) >  0
082300060809     c                   eval      RDEPOT   = %subst(Dato_sk(sk):1:+
082400060809     c                                              LunDato_sk(sk))
082500060808     c                   End
082600060808      **
082700060808      * ? Nel 32° campo : Parcel Number                              PKNR
082800060808      **                                                             PARCEL NR
082900060808     C                   add       1             sk
083000060808     c                   If        LunDato_sk(sk) >  0
083100060808     c                   eval      PKNR     = %subst(Dato_sk(sk):1:+
083200060808     c                                              LunDato_sk(sk))
083300060809      * ?   ( Il parcel non è + numerico )   controlla lunghezza del Parcel
083400060809     c                   clear                   Lung_Parcel
083500060809     c                   eval      Lung_Parcel = %Len(%trim(PKNR))
083600060809     c                   if        Lung_Parcel = 11
083700060809     C                   MOVEL     PKNR          parcel11
083800060809     c                   end
083900060809     c                   if        Lung_Parcel = 14
084000060809     C                   MOVEL     PKNR          parcel14
084100060809     c                   end
084200060809      **
084300060808     c                   End
084400060808      **
084500060808      * ? Nel 33° campo : Status Digit                               STATUS
084600060808      **                                                             STATUS
084700060808     C                   add       1             sk
084800060808     c                   If        LunDato_sk(sk) >  0
084900060808     c                   eval      STATUS   = %subst(Dato_sk(sk):1:+
085000060808     c                                              LunDato_sk(sk))
085100060808     c                   End
085200060808      **
085300060808      * ? Nel 34° campo : Cancellation Digit                         IDENT
085400060808      **                                                             CANCELLATO
085500060808     C                   add       1             sk
085600060808     c                   If        LunDato_sk(sk) >  0
085700060808     c                   eval      IDENT    = %subst(Dato_sk(sk):1:+
085800060808     c                                              LunDato_sk(sk))
085900060808     c                   End
086000060808      **
086100060808      * ? Nel 35° campo : Riferimento Cliente                        CUSTREF
086200060808      **                                                             RIFERIMENTO CLIENTE
086300060808     C                   add       1             sk
086400060808     c                   If        LunDato_sk(sk) >  0
086500060808     c                   eval      CUSTREF  = %subst(Dato_sk(sk):1:+
086600060808     c                                              LunDato_sk(sk))
086700061002     c     minu:maiu     XLATE     CUSTREF       CUSTREF_maiu                   *Minus -> Maiuscolo
086800060808     c                   End
086900060808      **
087000060808      * ? Nel 36° campo : Numero Ordine WEB                          WEBNR
087100060808      **                                                             ORDINE WEB
087200060808     C                   add       1             sk
087300060808     c                   If        LunDato_sk(sk) >  0
087400060808     c                   eval      WEBNR    = %subst(Dato_sk(sk):1:+
087500060808     c                                              LunDato_sk(sk))
087600060808     c                   End
087700060808      **
087800060808      * ? Nel 37° campo : Data Creazione Ordine                      NEUDAT
087900060808      **                                                             DATA CREAZIONE ORDINE
088000060808     C                   add       1             sk
088100060808     c                   If        DatoNum_sk(sk) = 'N' and
088200060808     c                             LunDato_sk(sk) >  0
088300060808      * ?-----> Errore
088400060809     C                   eval      tipo_error = 3
088500060809     C                   exsr      Field_Error
088600060808     c                   Else
088700060808     c                   eval      NEUDAT   = atoi(%TRIM(Dato_sk(sk)))
088800060808     c                   End
088900060808      **
089000060808      * ? Nel 38° campo : Data Generazione                           AENDDAT
089100060808      **                                                             DATA GENERAZIONE
089200060808     C                   add       1             sk
089300060808     c                   If        DatoNum_sk(sk) = 'N' and
089400060808     c                             LunDato_sk(sk) >  0
089500060808      * ?-----> Errore
089600060809     C                   eval      tipo_error = 3
089700060809     C                   exsr      Field_Error
089800060808     c                   Else
089900060808     c                   eval      AENDDAT  = atoi(%TRIM(Dato_sk(sk)))
090000060808     c                   End
090100060809      **
090200060809      * ? Nel 39° campo : SERVICE CODE                               SERVICE
090300060809      **                                                             SERVICE CODE
090400060809     C                   add       1             sk
090500060809     c                   If        DatoNum_sk(sk) = 'N' and
090600060809     c                             LunDato_sk(sk) >  0
090700060809      * ?-----> Errore
090800060809     C                   eval      tipo_error = 3
090900060809     C                   exsr      Field_Error
091000060809     c                   Else
091100060809     c                   eval      SERVICE  = atoi(%TRIM(Dato_sk(sk)))
091200060809     c                   End
091300060809      **
091400060809      * ? Nel 40° campo : CUSTOM  status code                        CUSTOM
091500060809      **                                                             CUSTOM S.CODE
091600060809     C                   add       1             sk
091700060809     c                   If        LunDato_sk(sk) >  0
091800060809      * ?-----> Errore
091900060809     c                   eval      CUSTOM   = %subst(Dato_sk(sk):1:+
092000060809     c                                              LunDato_sk(sk))
092100060809     c                   End
092200060808      **
092300060809      * ? Nel 41° campo : Informazione 1                             INFO1
092400060808      **                                                             INFORMAZIONE 1
092500060808     C                   add       1             sk
092600060808     c                   If        LunDato_sk(sk) >  0
092700060808     c                   eval      INFO1    = %subst(Dato_sk(sk):1:+
092800060808     c                                              LunDato_sk(sk))
092900061002     c     minu:maiu     XLATE     INFO1         INFO1_maiu                     *Minus -> Maiuscolo
093000060808     c                   End
093100060808      **
093200060809      * ? Nel 42° campo : Informazione 2                             INFO2
093300060808      **                                                             INFORMAZIONE 2
093400060808     C                   add       1             sk
093500060808     c                   If        LunDato_sk(sk) >  0
093600060808     c                   eval      INFO2    = %subst(Dato_sk(sk):1:+
093700060808     c                                              LunDato_sk(sk))
093800061002     c     minu:maiu     XLATE     INFO2         INFO2_maiu                     *Minus -> Maiuscolo
093900060808     c                   End
094000060808      **
094100060809      * ? Nel 43° campo : Peso (Con decimali)                        WEIGHT
094200060808      **                                                             PESO IN KG
094300060808     C                   add       1             sk
094400060808     c                   If        DatoNum_sk(sk) = 'N' and
094500060808     c                             LunDato_sk(sk) >  0
094600060808      * ?-----> Errore
094700060809     C                   eval      tipo_error = 3
094800060809     C                   exsr      Field_Error
094900060808     c                   Else
095000060808     c                   eval      WEIGHT   = atof(%TRIM(Dato_sk(sk)))
095100060808     c                   End
095200060808      **
095300060809      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL FIDPODTA1
095400060612      **
095500060612     c                   Endsr
095600060614      * ?------------------------------------------------------------------ */
095700060614      *?      Campi in errore
095800060614      * ?------------------------------------------------------------------ */
095900060614     c     Field_Error   Begsr
096000060614      **
096100060614     C                   eval      se_errore   = 'S'
096200060614      * msg video o in posta
096300060614     c                   If        Msg_Err = *blank
096400060614     c
096500060614     c                   Select
096600060614     c
096700060614     c                   When      Tipo_Error = 1
096800060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
096900060614     C                             ' >> Obblig.- Manca Dato !!'
097000060614     c
097100060614     c                   When      Tipo_Error = 2
097200060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
097300060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
097400060614     c
097500060614     c                   When      Tipo_Error = 3
097600060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
097700060614     C                             ' >> NON Numerico  !!'
097800060614     c
097900060614     c                   When      Tipo_Error = 4
098000060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
098100060614     C                             ' >> Conversione non riuscita !!'
098200060614     c                   EndSL
098300060614     c
098400060614     c                   End
098500060614      * Errore su VINMSG
098600060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
098700060614     c                                          + %trim(Testa_sk(sk))
098800060614      **
098900060614     c                   Endsr
099000060808      * ?------------------------------------------------------------------ */
099100060808      *?    Scrive i Files degli ORM
099200060808      * ?------------------------------------------------------------------ */
099300060809     c     WRT_FILE      Begsr
099400060808      **
099500060808      **  se richiesta cancellazione di un Collection Request precedente
099600060808     c                   if        IDENT = '3'
099700060808      **?              /*---------------------- */
099800060808     c                   ExSr      STORNO
099900060808      * ?              /*---------------------- */
100000060808     c                   else
100100080430      **
100200080430      **  controlla sul FIDPO se era già arrivato il depot/nr.ordine x errore DPD
100300080430      **  o se si stanno reintroducendo erroneamente gli stessi dati.
100400080430      **   ATTENZIONE !!!!
100500080430     C                   eval      DP3DEP   = ODEPOT
100600080430     C                   eval      DP3ORD   = ORDERNR
100700080430     c     dp3_Ordine    setll     Fidpo03l
100800080430      **
100900080430      **?              /*- se non c'è è OK   -- */
101000080430     c                   if        not %equal
101100080430      **
101200060808      **  Se è in questo punto significa che non sono stati trovati errori
101300080430      **    formali quindi si passa alla scrittura
101400080430      **   e del FIDPO00F con solo l'Ordine DPD impostato
101500080430      **?              /*---------------------- */
101600080430     c                   ExSr      Crea_FIDPO
101700080430      * ?              /*---------------------- */
101800080430      **   e dell'FNVAO000 per impostare l'ORM dal Collection Request
101900060808      **?              /*---------------------- */
102000060808     c                   ExSr      Crea_VAO
102100060808      * ?              /*---------------------- */
102200080430      *
102300080430     c                   else
102400080430      **
102500080430      **?              /*-  altrimenti MSG errore */
102600080430     c                   eval      se_errore ='S'
102700080430     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)
102800080430     c                               + '; ordine già ricevuto da DPD'
102900080430      **
103000080430     c                   end
103100080430      *
103200060808     c                   end
103300060809
103400060809     c                   endSR
103500060808      * ?------------------------------------------------------------------ */
103600060808      *?    Imposta i campi per il VAO
103700060808      * ?------------------------------------------------------------------ */
103800060808     c     Crea_FIDPO    Begsr
103900060808      **
104000060808     c                   clear                   fidpo000
104100060808     C* ?Nr.ORM  DPD
104200060808     C                   eval      DPODEP   = ODEPOT
104300060808     C                   eval      DPOORD   = ORDERNR
104400060808     C                   eval      DPODTRIC = data_oggi
104500080718      *
104600080718     C* ?Nei primi 4 bytes del Filler impostiamo il DEPOT di RITIRO
104700080718     C* ?Importante x DPD quando restituiamo il CRDATA02 :
104800080718     C* ?Deve essere ASSOLUTAMENTE identico nel CRDATA02 il CDEPOT
104900080718     C* ?altrimenti DPD non riesce a dare le informazioni correttamente al
105000080718     C* ?Depot che ha ordinato il ritiro... quindi dirgli qual'è il PARCEL
105100080718     C* ?che è stato ritirato e che arriverà in consegna e quindi che potrà
105200080718     C* ?vedere nell'Iter del Track & Trace.
105300080718     c                   movel     CDEPOT        dpoFLR
105400080718     C* ?quindi attenzione al Filler
105500080718      *
105600060808     c                   write     fidpo000
105700060808      **
105800060808     c                   Endsr
105900060808      * ?------------------------------------------------------------------ */
106000060808      *?    Imposta i campi per il VAO
106100060808      * ?------------------------------------------------------------------ */
106200060808     c     Crea_VAO      Begsr
106300060808      **
106400060808     c                   clear                   fnvao000
106500060808     c                   clear                   err_bloccante     1
106600060914      **
106700060914      ** tipo comunicazione ORM
106800060914     C                   eval      vaotco = 'F'
106900060808      **
107000061002     C                   eval      vaorsr = CNAME_maiu
107100061002     C                   eval      vaoinr = CSTREET_maiu
107200060808     C                   eval      vaocar = CPOSTAL
107300061002     C                   eval      vaolor = CCITY_maiu
107400061129     C                   eval      vaoter = CPHONE
107500061129     C                   eval      vaorer = CNAME2_maiu
107600061006     C*
107700061006     C* CAP OBBLIGATORIO : CONTROLLO
107800061006     C                   CLEAR                   DSSI95
107900061006     C                   CLEAR                   DSLV13
108000061006     C                   MOVEL     '3'           I95TCN
108100061006     C                   MOVEL     vaocar        I95CAP
108200061006     C                   MOVEL     vaolor        I95LOC
108300061006     C* ERRORE AFFIDABILITA = 0
108400061006     C                   MOVEL     'S'           I13AF0
108500061006     C                   MOVEL     'S'           I13AF1
108600080430     C                   z-add     1             E13FZ1
108700061006     C                   CALL      'FNLV13R'
108800061006     C                   PARM                    KPJBA
108900061006     C                   PARM                    DSLV13
109000061006     C                   PARM                    DSSI95
109100061006     C* ERRORE
109200061006     C     O13ERR        IFEQ      *BLANKS
109300061006     C                   MOVEL     O95PRV        vaoPRR
109400061006    3C                   ENDIF
109500061006     C*
109600060808     C* Nazione dove effettuare il Ritiro
109700060808     C                   z-add     1             Nz
109800061002     C     cCountry      lookup    Cie03(Nz)                              99
109900080404DO  1C                   if        %Equal
110000060808     C                   MOVEL     Naz03(Nz)     vaoNAR
110100060808E   1C                   End
110200060808      *
110300061002     C                   eval      vaorsc = RNAME_maiu
110400061002     C                   eval      vaoinc = RSTREET_maiu
110500060808     C                   eval      vaocac = RPOSTAL
110600061002     C                   eval      vaoloc = RCITY_maiu
110700061006     C*
110800081031     C*  SE IL DESTINATARIO È EXAPAQ località "LIEUSAINT CEDEX" cap fittizio 00036
110900081031     C*   si deve sostituire il cap con "77564" CAP reale .
111000081031     C*  Richiesta di DPD da parte della Villa confermato con mail del 31/10/2008.
111100081031     C*
111200081031     C*  indirizzo destinatario:
111300081031     C*    DEposito 1003.
111400081031     C*    EXAPAQ SA
111500081031     C*    BOULEVARD DU LUXEMBOURG ZAC PARISUD
111600081031     C*    (cap fittizio 00036) cap vero 77564     LIEUSAINT CEDEX         F
111700081031     C*                                 =======
111800081031     c                   clear                   LieuSced         15
111900081031     C                   eval      LieuSced = 'LIEUSAINT CEDEX'
112000081031     c     LieuSced      scan      vaoloc                                 99
112100081031     C                   If        *in99
112200081031     c     '00036'       scan      vaocac                                 99
112300081031     C                   If        *in99
112400081031     c                   eVAl      vaoCAC = '77564'
112500081031     C                   EnD
112600081031     C                   EnD
112700081031     C*
112800061006     C* CAP OBBLIGATORIO : CONTROLLO
112900061006     C                   CLEAR                   DSSI95
113000061006     C                   CLEAR                   DSLV13
113100061006     C                   MOVEL     '3'           I95TCN
113200061006     C                   MOVEL     vaocac        I95CAP
113300061006     C                   MOVEL     vaoloc        I95LOC
113400061006     C* ERRORE AFFIDABILITA = 0
113500061006     C                   MOVEL     'S'           I13AF0
113600061006     C                   MOVEL     'S'           I13AF1
113700080430     C                   z-add     1             E13FZ1
113800061006     C                   CALL      'FNLV13R'
113900061006     C                   PARM                    KPJBA
114000061006     C                   PARM                    DSLV13
114100061006     C                   PARM                    DSSI95
114200061006     C* ERRORE
114300061006     C     O13ERR        IFEQ      *BLANKS
114400061006     C                   MOVEL     O95PRV        vaoPRc
114500061006    3C                   ENDIF
114600061006     C*
114700060808     C* Nazione di Consegna
114800060808     C                   z-add     1             Nz
114900061002     C     rCountry      lookup    Cie03(Nz)                              99
115000080404DO  1C                   if        %Equal
115100060808     C                   MOVEL     Naz03(Nz)     vaoNAC
115200060808E   1C                   End
115300060808      *
115400060808     C                   eval      vaodao = Data_Trs_UNO
115500060808     C                   eval      vaooao = Time_Trs_UNO
115600061002     C                   eval      vaono1 = INFO1_maiu
115700061002     C                   eval      vaono2 = INFO2_maiu
115800120528      **
115900120528      *** Su richiesta di Christian Burke: per quanto riguarda il referente di ritiro
116000120528      **   viene spesso impostato sul loro campo INFO1 e quindi la richiesta è di
116100120528      **   riportarlo nel nostro file come referente.
116200120528     C                   eval      vaorer = %trim(vaorer) +' '+ INFO1_maiu
116300120528     C                   eval      vaorer = %trim(vaorer) +' '+ INFO2_maiu
116400120528      ***
116500120528      **
116600060808     C                   eval      vaopkg = WEIGHT
116700080512      * se non ricevuto il peso imposta 11kg.
116800080512     c                   if        vaopkg = 0
116900080512     c                   z-add     11            vaopkg
117000080512     c                   end
117100080512      * imposta fisso le 12:00 come ora di ritiro
117200080512     c                   z-add     1200          vaoorr
117300060808     C                   eval      vaoncl = 1
117400060808     C                   eval      vaotor = 'S'
117500060808     C*-------------------------
117600080527     C* ?Si era deciso che gli ORM dovevano essere presi tutti con linea 190
117700080527     C                   z-add     190           vaoPOE
117800080527     C                   z-add     190           vaoPOC
117900140930      **----
118000140929      **  Aggancia la LINEA dal "Depot ORDINANTE" e se NON riesce segue la vecchia
118100140929      **  strada della NAZ ORDinante (eccezione x l'Italia)
118200140930     c                   z-add     0             in_POE            3 0
118300140930      *
118400140929      **               *-------------------------*
118500140929     c                   exsr      NAZ_da_oDepot
118600140929      **               *-------------------------*
118700140930      *
118800140930      ** Se ha trovato la linea
118900140930      **  dalla Routine dei files del Cappario imposta questa sul POE
119000140930     c                   if        in_POE > 0
119100140930     C                   z-add     in_POE        vaoPOE
119200140930      **  altrimenti esegue le vecchie specifiche
119300140930      **   con cui impostare il codice POE
119400140930     c                   else
119500140929      **
119600080708     C*   in base alla Nazione del Depot Ordinante ricava Torino/Verona
119700060808     C                   z-add     1             Nz
119800140924      ** solo se l'ordinante è Italia, con la Nazione di Destino cerca di
119900140924      **  decodificare la corretta linea di partenza
120000140924      **  che normalmente deve essere sempre decodificata con la Naz.Ordinante
120100140924     c                   if        oCountry = 380
120200140924     C     rCountry      lookup    Cie03(Nz)                              99
120300140924     c                   else
120400140924      *-
120500140924      *- normalmente deve cercare con Ocountry.
120600080708     C     oCountry      lookup    Cie03(Nz)                              99
120700140924      *-
120800140924E   1C                   End
120900140924      *-
121000080708DO  1C                   if        %Equal and POE03(Nz) > 0
121100080604     C                   MOVEL     POE03(Nz)     vaoPOE
121200080527E   1C                   End
121300080708      *
121400140930     c                   endIF
121500140930      **----
121600080708      **  se Italia la destinazione di Consegna non imposto il P.O.di Consegna
121700080708     c                   if        rCountry = 380
121800080708     C                   z-add     0             vaoPOC
121900080708     c                   else
122000080708      **
122100080708     C*   in base alla Nazione di Consegna ->linea DPD x P.O. di Consegna
122200080708     C                   z-add     1             Nz
122300080708     C     rCountry      lookup    Cie03(Nz)                              99
122400080708     c                   if        %Equal and DpD03(Nz) > 0
122500080527     C                   MOVEL     DpD03(Nz)     vaoPOC
122600080527E   1C                   End
122700080708      **
122800080708     c                   end
122900080521      **
123000060808     C*--------------------
123100060808     C* ?Nr.ORM  DPD su riferimento Alfanumerico
123200060809     c                   move      ODEPOT        ordDep4           4
123300060808     c                   move      ORDERNR       ordNum6           6
123400060811     C                   eval      VAORFA = 'DPD' + ordDep4 + ordNum6
123500060808     C*--------------------
123600060808     C                   eval      VAOPAG = 'O'
123700060808     C* ?Codice Ordinante
123800060925     C* ? reperisce il codice cliente in base al depot ordinante
123900060925     C* ? e se non lo trova cerca di impostarlo con il default sulla PT
124000060925     C* ? per il codice DPD190
124100060925     C                   clear                   TRULDEPds
124200060925     c                   move      'L'           IdepTLA
124300060925     c                   move      Odepot        IdepDPC
124400060925     c                   call      'TRULDEPR'
124500060925     C                   PARM                    TRULDEPds
124600090522      *
124700061129     C                   IF        OdepERR <>'1'  and OdepKSC >0
124800060925     c                   move      OdepKSC       cod07n            7 0
124900060925     c                   else
125000090522      *
125100060925     c                   z-add     §ptKSC        cod07n            7 0
125200090522      *
125300090522      ** avvisa REA e INT + Bertocchi x Legame non trovato
125400090522     c                   exsr      email_alert
125500060925     c                   end
125600060925      **
125700060808     c                   z-add     0             cod10n           10 0
125800060808     c                   movel     Cod07n        cod10n
125900060925      **
126000060808     c                   movel     'N'           trova_cod         1
126100060808      **
126200060808     C* ?Decodifica  Codice Ordinante su Anagrafica clienti di Ritiro
126300060814     C* ?------------------
126400060911      *?    DA SOSTITUIRE APPENA pronta utility per reperire da DPPDC00F
126500060808     C     cod10n        setll     Fnacr01l
126600060808     C                   read      Fnacr01l
126700060808      **
126800060808     c                   Dow       not %EoF(Fnacr01l)
126900061129     c                   movel     ACRCRO        par07n            7 0
127000060808      **
127100060808      **  Se diverso deve uscire immediatamente
127200060808     c                   If        par07n <> cod07n
127300060808     c                   leave
127400060808     c                   Else
127500060808      **  Se uguale invece deve essere non bloccato (M)
127600060808     c                   iF        ACRTCR <> 'M'
127700060808     c                   movel     'S'           trova_cod
127800060808     c                   leave
127900060808     c                   endiF
128000060808     c                   endIf
128100060808      **
128200060808     C                   read      Fnacr01l
128300060808     c                   endDO
128400060814     C* ?------------------
128500060808      **
128600060808     c                   if        trova_cod = 'S'
128700060808      ** se ha decodificato l'ordinante cerca sull'anagrafica dei ritiri
128800060808     c                   move      *zeros        vaoCOR
128900060808     c                   movel     ACRCRO        vaoCOR
129000060808     c                   else
129100060808     c                   z-add     1             acokut
129200060808     c                   z-add     151           acokcc
129300060808     c                   z-add     Cod07n        acoksc
129400060808     C     KAco          chain     Cnaco00F
129500060808     c                   if        %found(Cnaco00F)
129600060808      ** se ha decodificato l'ordinante cerca sull'anagrafica dei ritiri
129700060808     c                   move      *zeros        vaoCOR
129800060808     c                   movel     Cod07n        vaoCOR
129900060808     C                   Else
130000060808     C*
130100061002     C                   eval      vaorso = ONAME_maiu
130200061002     C                   eval      vaoino = OSTREET_maiu
130300060808     C                   eval      vaocao = OPOSTAL
130400061002     C                   eval      vaoloo = OCITY_maiu
130500061006     C* CAP OBBLIGATORIO : CONTROLLO
130600061006     C                   CLEAR                   DSSI95
130700061006     C                   CLEAR                   DSLV13
130800061006     C                   MOVEL     '3'           I95TCN
130900061006     C                   MOVEL     vaocao        I95CAP
131000061006     C                   MOVEL     vaoloo        I95LOC
131100061006     C* ERRORE AFFIDABILITA = 0
131200061006     C                   MOVEL     'S'           I13AF0
131300061006     C                   MOVEL     'S'           I13AF1
131400080430     C                   z-add     1             E13FZ1
131500061006     C                   CALL      'FNLV13R'
131600061006     C                   PARM                    KPJBA
131700061006     C                   PARM                    DSLV13
131800061006     C                   PARM                    DSSI95
131900061006     C* ERRORE
132000061006     C     O13ERR        IFEQ      *BLANKS
132100061006     C                   MOVEL     O95PRV        vaoPRo
132200061006    3C                   ENDIF
132300061006     C*
132400060808     C* Nazione Ordinante
132500060808     C                   z-add     1             Nz
132600061002     C     oCountry      lookup    Cie03(Nz)                              99
132700080404DO  1C                   if        %Equal
132800060808     C                   MOVEL     Naz03(Nz)     vaoNAO
132900060808E   1C                   End
133000060808     C*
133100060808     C                   End
133200060808     C                   End
133300060808      **
133400060808      **  ORM Commissionato
133500060808     c                   move      'S'           §orComC
133600060808     c                   move      'S'           §ormFD
133700080423     c                   move      'S'           §ormFR
133800080423     c*
133900060808     c                   eval      vaoFLO = Dorm01
134000061002     C* ?Appunto se DPD fa ORM commissionato:             --> il flag = "S"
134100061002     C* ?  DPD deve inviare per ORM commissionato delle informazioni precise:
134200061002     C* ?  - Peso del collo.
134300061002     C* ?  - Ora dalla quale il nostro Autista può andare a ritirare
134400061002     C* ?  - Il Referente da contattare
134500061002     C* ?  - Il Telefono del Referente
134600061002     C* ?Se invece l'Orm non Fosse commissionato, significa che già DPD ha preso i
134700061002     C* ? contatti e noi dobbiamo solo andare a ritirare. --> il flag = "N"
134800060808      **
134900160215      **   CHIODO:
135000160215      **     x correggere se in consegna in PORTOGALLO e con CAP inferiore a 7 caratteri
135100160215      **     imposta il codice completandolo con degli zeri dopo il codice (quindi alla dx)
135200160215     c                   if        vaoNAC = 'P  '
135300160215     c                               and
135400160215     c                             %len(%trim(vaoCAC)) < 7
135500160215      **  riempe con '0'  es.:XXXX000
135600160215     c                   z-add     0             da                1 0
135700160215     c                   z-add     0             in                1 0
135800160215      *
135900160215     c                   eval      da = %len(%trim(vaoCAC)) + 1
136000160215     c     da            do        7             in
136100160215     c                   eval      %subst(vaoCAC:in:1)='0'
136200160215     c                   Enddo
136300160215     c                   End
136400160215      **
136500060808     c                   write     fnvao000
136600060808     c                   move      'S'           Almeno_Uno
136700060808      **
136800060808     c                   Endsr
136900140929      * ?------------------------------------------------------------------ */
137000140929      *?   Cerca la LINEA TO o VR da
137100140929      * ?------------------------------------------------------------------ */
137200140929     c     NAZ_da_oDepot Begsr
137300171106      *
137400171106     c                   clear                   oDepot7           7
137500171106      * trascodifico la B.U.
137600171106      * recupero  versione di cappario
137700171106     c                   z-add     data_oggi     ISID1DRI
137800171106     C                   clear                   OSID1VER
137900171106     C                   clear                   OSID1VERD
138000171106     C                   clear                   OSID1DDE
138100171106     C                   clear                   OSID1DSC
138200171106     C                   clear                   OSID1BCID
138300171106     c                   call      'TISID1R'
138400171106     C                   PARM                    ISID1DRI
138500171106     C                   PARM                    OSID1VER
138600171106     C                   PARM                    OSID1VERD
138700171106     C                   PARM                    OSID1DDE
138800171106     C                   PARM                    OSID1DSC
138900171106     C                   PARM                    OSID1BCID
139000171106      ** ------
139100171106      * se TROVATA VERSIONE CAPPARIO
139200171106      * prova a decodificare il DEPOT con il nuovo cappario per reperire la BU
139300171106     c                   if        osid1ver > 0
139400171106     c                   move      *blank        CDPATB
139500171106     c                   z-add     OSID1VER      CDPVER
139600171106     c                   movel     oDepot        CDPDSTR
139700171107     c                   clear                   ISOnazCAP         3
139800171106     c     Kcdp4         chain     dpcdp12i
139900171106      *   il Depot con la B.U.
140000171106     c                   if        %Found(dpcdp12i)
140100171106     c                   movel     CDPDPTN       oDepot7
140200171107     c                   movel     CDPISO2       ISOnazCAP
140300171106     c                   else
140400171106      * depot che non esiste
140500171106     c                   movel     *all'0'       oDepot7
140600171106     c                   move      oDepot        oDepot7
140700171106     c                   end
140800171106     c                   end
140900171106      ** ------
141000140929      /free
141100140930          // Sul cappario nella versione in vigore alla data
141200140930          // il Depot appartiene alla Nazione xxx
141300140930
141400171107                  IF ISOnazCAP <> *blank;
141500140930                       eval Nz = 1;
141600171107                       Nz = %LOOKUP(ISOnazCAP:iso03);
141700140930
141800140930            // se ha trovato imposta il campo di passaggio codice x il POE
141900140930                            If %Equal and POE03(Nz) > 0;
142000140930                                eval in_POE = POE03(Nz);
142100140930                            endif;
142200140930
142300140930                       leavesr;
142400140929                  ENDIF;
142500140929      /end-free
142600140929      **
142700140929     c                   Endsr
142800060808      * ?------------------------------------------------------------------ */
142900060808      *?   Se richiesto di stornare un ORM
143000060808      * ?------------------------------------------------------------------ */
143100060808     c     STORNO        Begsr
143200060808      **
143300060808     c                   movel     ODEPOT        Key_DPDORM
143400060808     c                   move      ORDERNR       Key_DPDORM
143500060808      **     pgm di storno ORDINI DPD
143600060808     c                   call      'FIEU46R'
143700060809     c                   parm                    Key_DPDORM       10
143800060808      **
143900060808     c                   Endsr
144000060612      * ?------------------------------------------------------------------ */
144100060612      *?      Suddivide i campi della riga con carattere divisorio (;)
144200060612      * ?------------------------------------------------------------------ */
144300060612     c     Split_Rec     Begsr
144400060612      **
144500060612     c                   clear                   Nr_campi
144600060612     c                   clear                   Campi_Record
144700060612     c                   clear                   Campi_Lunghi
144800060612     c                   clear                   Campi_Numerici
144900060612     c                   clear                   Campi_Decimali
145000060612     c                   movel     ';'           Separa_campi
145100060612     c                   movel     '.'           Decimal_Separ
145200060612      *
145300060612     c                   call      'FIEU00R'
145400060612      * ?     Input
145500060612     c                   parm                    Separa_campi
145600060612     c                   parm                    Decimal_Separ
145700060612     c                   parm                    dati
145800060612      * ?     Output  su schiere di 100 elementi
145900060612     c                   parm                    Nr_campi
146000060612     c                   parm                    Campi_Record                   * i dati
146100060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
146200060612     c                   parm                    Campi_Numerici                 * se numerici
146300060612     c                   parm                    Campi_Decimali                 * quanti decimali
146400060612      **
146500060612     c                   Endsr
146600971215      *----------------------------------------------------------------
146700051205      *   DEFINIZIONE CHIAVI                               *
146800060808      *----------------------------------------------------------------
146900051205     C     *INZSR        BEGSR
147000051205      *
147100991129     c     *ENTRY        PLIST
147200060613     C                   parm                    esito             1
147300991129      *
147400060808     C     Key_Tbl       klist
147500060808     c                   kfld                    tblKUT
147600060808     c                   kfld                    tblCOD
147700060808     c                   kfld                    tblKEY
147800060808      *
147900060808     C     Key_Tbl1      klist
148000060808     c                   kfld                    tblKUT
148100060808     c                   kfld                    tblCOD
148200060808      *
148300060808     C     KAco          Klist
148400060808     c                   Kfld                    acokut
148500060808     c                   Kfld                    acokcc
148600060808     c                   Kfld                    acoksc
148700060925      *
148800060925     C     Kpt190        Klist
148900060925     c                   Kfld                    tabCOD
149000060925     c                   Kfld                    tabKEY
149100080430      *
149200080430     C     dp3_Ordine    Klist
149300080430     c                   Kfld                    Dp3DEP
149400080430     c                   Kfld                    Dp3ORD
149500171106      **
149600171106     c     Kcdp4         klist
149700171106     c                   kfld                    CDPATB
149800171106     c                   kfld                    CDPVER
149900171106     c                   kfld                    CDPDSTR
150000060808      *
150100060808      *  imposta UDATE di elaborazione
150200060915     C                   move      *date         data_oggi
150300060915     C                   move      *date         G02inv
150400060915     C                   MOVE      *ZEROS        G02dat
150500060808     C                   MOVE      *BLANKS       G02ERR
150600060808     C                   CALL      'XSRDA8'
150700060808     C                   PARM                    WLBDAT
150800060808     C                   move      data_oggi     day
150900060808     C                   TIME                    W0140
151000060808     C                   MOVEl     W0140         ore
151100060808      *
151200060808      * Nazione di consegna x linea DPD x P.O. di Consegna ed Emittente
151300060808     C                   z-add     0             Nz                3 0
151400060808     c                   clear                   Naz03
151500060808     c                   clear                   Iso03
151600060808     c                   clear                   DpD03
151700060808     C                   z-add     1             tblKUT
151800060808     C                   MOVEL(p)  '15'          tblCOD
151900060808     C     Key_Tbl1      setll     TABEL00F
152000060808     C     Key_Tbl1      reade     TABEL00F
152100060808DO  1C                   Dow       not %EoF(TABEL00F)
152200060808DO  1C                   if         tblflg = *blank
152300060808     C                   MOVEL     TBLuni        DS15
152400060808     C                   add       1             Nz
152500060808     C                   MOVEL     tblKEY        Naz03(Nz)
152600060808     C                   MOVEL     §15COD        Iso03(Nz)
152700060808     C                   MOVEL     §15LAD        DpD03(Nz)
152800060809     C                   MOVEL     §15CIE        Cie03(Nz)
152900080604     C                   MOVE      *zeros        POE03(Nz)
153000080604     c                   if        §15dpdPOE <> *blank
153100080604     c     digits        check     §15dpdPOE
153200080528     c                   if        not %Found
153300080604     C                   MOVE      §15dpdPOE     POE03(Nz)
153400080604E   1C                   End
153500080604     c                   if        POE03(Nz) = 0
153600080604     C                   z-add     190           POE03(Nz)
153700080528E   1C                   End
153800080527E   1C                   End
153900060808E   1C                   End
154000060808     C     Key_Tbl1      reade     TABEL00F
154100060808E   1C                   End
154200050414      *
154300080505      * decodifica tabella PT
154400080505     c                   clear                   edidsPT
154500080505     c                   move      'PT'          tabCOD
154600080505     c                   movel(p)  'DPD190'      tabKEY
154700080505     c     Kpt190        chain     EDtab01l
154800080505     c                   if        %Found(Edtab01l)
154900080505     c                   movel     tabUNI        edidsPT
155000080505     c                   end
155100080505      *
155200991124     C                   ENDSR
155300060621      * ?------------------------------------------------------------------ */
155400090522      *?  Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
155500060621      * ?------------------------------------------------------------------ */
155600090522     C     eMail_alert   BEGSR
155700090601     C*
155800090601     C                   eval      wrkOgg='Depot ' + Odepot +' NON presente sul-
155900090601     C                             la gestione Depot DPD/Codice Cliente.'
156000090601     C*
156100090601     C                   eval      wrkMsg='E'' stato ricevuto un ORM di DPD da'+
156200090601     C                             'l Depot ' + ODEPOT + '.   Attenzione, il ' +
156300090601     C                             'Depot non è presente sulla nostra tabella '+
156400090601     C                             'dei Lagami Depot/Codice Cliente. '         +
156500090601     C                             'Utilizzando la funzione: ( '               +
156600090601     C                             'Gestione legami Depot DPD/Codice Cliente )'+
156700090601     C                             ', inserire il legame per il Depot '        +
156800090601     C                             'Mancante.  Al momento l''ORM chiesto dal ' +
156900090601     C                             'DEPOT ' + ODEPOT + ' prenderà il cliente ' +
157000090601     C                             'Ordinante generico.'
157100090601     C*
157200090601     c                   if        VaoPOE = 190
157300121129     C                   eval      wrkEml   = 'INT043@brt.it'
157400121129     C                   eval      wrkEmlcc = 'REA043@brt.it'
157500150325     c                   elseif    VaoPOE = 194
157600150325     C                   eval      wrkEml   = 'INT002@brt.it'
157700150325     C                   eval      wrkEmlcc = 'REA002@brt.it'
157800180226     c                   elseif    VaoPOE = 295
157900180226     C                   eval      wrkEml   = 'INT116@brt.it'
158000180226     C                   eval      wrkEmlcc = 'REA116@brt.it'
158100180226     c                   elseif    VaoPOE = 296
158200180226     C                   eval      wrkEml   = 'INT116@brt.it'
158300180226     C                   eval      wrkEmlcc = 'REA116@brt.it'
158400180226     c                   elseif    VaoPOE = 195
158600121129     C                   eval      wrkEml   = 'INT049@brt.it'
158700121129     C                   eval      wrkEmlcc = 'REA049@brt.it'
158800090601     c                   end
158900090601      *
159000121129     C                   call(e)   'TIS701C1'
159100090601     C                   parm                    wrkEml          253
159200121129     C                   parm                    wrkEmlCc        253
159300090601     C                   parm                    wrkOgg           44
159400090601     C                   parm                    wrkMsg         5000
159500121129     C                   parm                    wrkesito          1
159600090522      *
159700090522     C                   ENDSR
159800090522      * ?------------------------------------------------------------------ */
159900090522      *?      X non bloccare in nessun caso il traduttore CLIENTI
160000090522      * ?------------------------------------------------------------------ */
160100090522     C     *pssr         BEGSR
160200090522     C
160300060710     C                   eval      esito = '2'
160400060621     C
160500060621     C                   ENDSR     '*CANCL'
160600060614      *-------------------------------------------------------------------------
