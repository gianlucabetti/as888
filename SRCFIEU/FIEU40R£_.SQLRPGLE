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
001900060808      *---------------------------------------------------------------------*
002000060808     D Ds15          e DS
002100060808     D  Naz03          s              3    DIM(999)
002200060808     D  Iso03          s              3    DIM(999)
002300060809     D  Cie03          s              3  0 DIM(999)
002400060808     D  Dpd03          s              3  0 DIM(999)
002500080604     D  POE03          s              3  0 DIM(999)
002600060808     D Dorm01        e DS
002700060925     D TRULDEPds     e DS
002800060925     D edidsPT       e DS
002900061006      *---------------------------------------------------------------------*
003000061006     D* DS PER FNLV13R - CONTROLLO DI UN CAP
003100061006     D DSLV13        E DS                  EXTNAME(FNLV13DS)
003200061006     D* DS PER TISI95R - CONTROLLO DI CAP
003300061006     D DSSI95        E DS                  EXTNAME(TISI95DS)
003400061006     D kpjba         e DS
003500060808      * ?================================================================== */
003600060612     D Lung_Parcel     S              3  0
003700060620     D Parcel11        S             11
003800060710     D Parcel14        S             14
003900060809     D Tipo_error      S              1  0
004000060808      * ?================================================================== */
004100060808     D  data_Oggi      s              8  0 INZ(0)
004200060808     D  day            s              8  0 INZ(0)
004300060808     D  ore            s              6  0 INZ(0)
004400060808     D  w0140          s             14  0 INZ(0)
004500060809      *
004600060808     D WLBDAT          DS
004700060808     D  G02DAT                 1      8  0
004800060808     D  G02INV                 9     16  0
004900060808     D  G02ERR                17     17
005000060808     D  G02TGI                18     22  0
005100060809      * ?   *--------------------------------------------------------------*
005200060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
005300060809      * ?   *--------------------------------------------------------------*
005400060809     D FIDPODTA1     e DS
005500061002     d  ONAME_maiu     s                   like(ONAME)
005600061002     d  ONAME2_maiu    s                   like(ONAME2)
005700061002     d  OSTREET_maiu   s                   like(OSTREET)
005800061002     d  OCITY_maiu     s                   like(OCITY)
005900061002     d  CNAME_maiu     s                   like(CNAME)
006000061002     d  CNAME2_maiu    s                   like(CNAME2)
006100061002     d  CNAME3_maiu    s                   like(CNAME3)
006200061002     d  CNAME4_maiu    s                   like(CNAME4)
006300061002     d  CSTREET_maiu   s                   like(CSTREET)
006400061002     d  CCITY_maiu     s                   like(CCITY)
006500061002     d  RNAME_maiu     s                   like(RNAME)
006600061002     d  RNAME2_maiu    s                   like(RNAME2)
006700061002     d  RSTREET_maiu   s                   like(RSTREET)
006800061002     d  RCITY_maiu     s                   like(RCITY)
006900061002     d  CUSTREF_maiu   s                   like(CUSTREF)
007000061002     d  INFO1_maiu     s                   like(INFO1)
007100061002     d  INFO2_maiu     s                   like(INFO2)
007200060612      * ?================================================================== */
007300060809      * ?   * Campi da tradurre con interi
007400060809     D atoll           PR            20I 0
007500060809     D                                     EXTPROC('atoll')
007600060809     D  Char                           *
007700060809     D                                     VALUE
007800060809     D                                     OPTIONS(*STRING)
007900060612      * ?   * Campi da tradurre con interi
008000060612     D atoi            PR            10I 0
008100060612     D                                     EXTPROC('atoi')
008200060612     D  Char                           *
008300060612     D                                     VALUE
008400060612     D                                     OPTIONS(*STRING)
008500060612      * ?   * Campi da tradurre con decimali
008600060612     D atof            PR             8F
008700060612     D                                     EXTPROC('atof')
008800060612     D  Char                           *
008900060612     D                                     VALUE
009000060612     D                                     OPTIONS(*STRING)
009100060612      * ?   *--------------------------------------------------------------*
009200060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
009300060612      * ?   *--------------------------------------------------------------*
009400060612      *
009500060612      * ?   * Campi x decodifica * (INPUT  del Record)
009600060612     D  Dati           s           1500
009700060612     D Separa_campi    s              1
009800060612     D Decimal_Separ   s              1
009900060809      *
010000060612      * ?   * Campi decodificati * (OUTPUT del Record)
010100060612      *       Schiere di Output / Campi di Output
010200060614     D  Alfa40         s             40
010300060612     d  Sk             s              3u 0
010400060612     D    NR_campi     s              3u 0
010500060612     D  Campi_Record   ds
010600060612     D    Dato_sk                    40    DIM(100)
010700060612     D  Campi_Lunghi   ds
010800060612     D    LunDato_sk                  3u 0 DIM(100)
010900060612     D Campi_Numerici  ds
011000060612     D    DatoNum_sk                  1    DIM(100)
011100060612     D Campi_Decimali  ds
011200060612     D    Decimal_sk                  3u 0 DIM(100)
011300060612      * ?   *--------------------------------------------------------------*
011400060612     D  position       s              3  0 INZ(0)
011500060612
011600060612     D  se_errore      s              1    inz(' ')
011700060612     D  Msg_Err        s            132    inz(' ')
011800060614     D  Msg_Vin_80     s             80    inz(' ')
011900060612      * ?* ------------------------------------------------------ *
012000060612      * Records di controllo mandati nell'IGATEAVIS
012100060612      * ?   * Dati Record UNO *
012200060612     DDepot_Trs_UNO    s             10
012300060612     D User_Trs_UNO    s             10
012400060612     D Data_Trs_UNO    s              8  0
012500060612     D Time_Trs_UNO    s              6  0
012600060612     D  Num_Trs_UNO    s              9  0
012700060612      * ?   * Dati Record DUE *
012800060809     D Vers_Trs_DUE    s             15
012900060809      * ?   * Dati Record TRE *
013000060809     D Testa_Sk        s             10    DIM(50)
013100060809      * ?   * Dati Record QUATTRO *
013200060612     D  Num_Trs_FIN    s              9  0
013300060710      **
013400061002     d minu            c                   const('qwertyuiopasdfghjklzxcvbnmàáâ-*alfabeto
013500061002     d                                     ãäåèéêëìíîïòóôõöùúûüæçðñßÑÀÁÂÃÄÅÆÈÉÊ-
013600061002     d                                     ËÌÍÎÏÒÓÔÕÖÙÚÛÜ')
013700061002     d maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMAAA-*ALFABETO
013800061002     d                                     AAAEEEEIIIIOOOOOUUUUACANSNAAAAAAAEEE-
013900061002     d                                     EIIIIOOOOOUUUU')
014000061002
014100060710     D Digits          C                   '0123456789'
014200060612      * ?================================================================== */
014300060612      *   Ciclo principale
014400060413      * ?********************************************************************/
014500060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
014600060612     C*
014700060612     C                   if        not %open(tivin00r)
014800060612     C                   open      tivin00r
014900060612     C                   endif
015000060612      **
015100060612      * ?------------------------------------------------------------------ */
015200060612     C*? Controllo dati arrivati da DPD
015300060612      * ?------------------------------------------------------------------ */
015400060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
015500060612      * ?- controllando che la trasmissione sia completa.
015600060612
015700060612      * ?              /*---------------------- */
015800060612     c                   exsr      check_Trasm
015900060612      * ?              /*---------------------- */
016000060612      **
016100060613      **  Errore di trasmissione x tutti i records
016200060613      **   --> file in errore
016300060613     c                   if        se_errore = 'S'
016400060612
016500060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
016600060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
016700060613     C                               >> Farsi rimandare i DATI !!'
016800060612
016900060612      * ?  Scrive su tutti i records il tipo di errore
017000060612     c     *start        setll     tivin00r
017100060612     c                   read      tivin00r
017200060612     c                   dow       not %eof(tivin00r)
017300060612     C                   eval      vinMSG = Msg_Err
017400060612     C                   eval      vinFLG = '2'
017500060613     c                   eval      esito  = '2'
017600060612     c                   update    tivin000
017700060612     c                   read      tivin00r
017800060612     c                   endDO
017900060612      **
018000060612     c                   else
018100060614      **
018200060612      * ?------------------------------------------------------------------ */
018300060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
018400060612      * ?------------------------------------------------------------------ */
018500060612      * ?              /*---------------------- */
018600060612     c                   exsr      Importa_Msg
018700060612      * ?              /*---------------------- */
018800060612
018900060614     c                   end
019000060614
019100060612     C                   if        %open(tivin00r)
019200060612     C                   close     tivin00r
019300060612     C                   endif
019400060612      *
019500060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
019600060614      *
019700060710     c                   if        almeno_uno ='S' and esito ='1'
019800060710     C                   eval      esito ='0'
019900060614     C                   endif
020000060614      *
020100060612     c                   SETON                                        LR
020200060612      * ?================================================================== */
020300060612      *? Controlla la trasmissione   se completa
020400060612      * ?------------------------------------------------------------------ */
020500060612     c     Check_Trasm   Begsr
020600060612
020700060612     C                   clear                   se_errore
020800060612      ** primo  record
020900060612     c     *start        setll     tivin00r
021000060612     c                   read      tivin00r
021100060612     c                   if        not %eof(tivin00r)
021200060612      * ?              /*---------------------- */
021300060612     c                   exsr      Decod_Record
021400060612      * ?              /*---------------------- */
021500060612     c                   endif
021600060612      **
021700060612      ** ultimo record
021800060612     c     *hival        setll     tivin00r
021900060612     c                   readp     tivin00r
022000060612     c                   if        not %eof(tivin00r)
022100060612      * ?              /*---------------------- */
022200060612     c                   exsr      Decod_Record
022300060612      * ?              /*---------------------- */
022400060612     c                   endif
022500060612
022600060612      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
022700060612      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
022800060612      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
022900060612     C                   if        num_trs_UNO <> num_trs_FIN
023000060612      * ?-----> Errore
023100060612     C                   eval      se_errore = 'S'
023200060612
023300060612     c                   end
023400060612
023500060612     c                   endSR
023600060612      * ?------------------------------------------------------------------ */
023700060612      *? Importa i records della tramsissione
023800060612      * ?------------------------------------------------------------------ */
023900060612     c     Importa_Msg   Begsr
024000060612
024100060614      * per controllare se almeno un record è stato importato sul VAB
024200060614     c                   clear                   Almeno_Uno        1
024300060614
024400060612     c     *start        setll     Tivin00r
024500060612     c                   read      Tivin00r
024600060612
024700060612     c                   dow       not %eof(Tivin00r)
024800060614
024900060614      * solo i record sflaggati da rielaborare
025000060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
025100060612
025200060612      ** Controlli formali sui campi
025300060612     c                   clear                   se_errore
025400060620     C                   clear                   Msg_Vin_80
025500060612
025600060612      ** Decodifica record a campi variabili
025700060612      * ?              /*---------------------- */
025800060612     c                   exsr      Decod_Record
025900060612      * ?              /*---------------------- */
026000060612
026100060612      **  Se presente un errore nel record emette una segnalazione msg
026200060612     c                   if        se_errore ='S'
026300060614     C                   eval      vinMSG = Msg_Vin_80
026400060621     c                   end
026500060612
026600060809      *  con il record in canna scrive direttamente il VAO
026700060620     c                   if        %subst(dati:1:1) <> '#'
026800060612      * ?              /*---------------------- */
026900060809     c                   exsr      WRT_FILE
027000060612      * ?              /*---------------------- */
027100060620     c                   end
027200060614
027300060809      *  Problemi nella decodifica dei campi VAO
027400060614     c                   if        se_errore ='S'
027500060614     C                   eval      vinMSG = Msg_Vin_80
027600060614     c                   end
027700060612
027800060621      *  Sempre Record OK
027900060621     C                   eval      vinFLG = '1'
028000060710     c                   eval      esito  = '0'
028100060621
028200060809      *  x errore bloccante nella composizione del VAO
028300060621     c                   if        err_bloccante ='S'
028400060621     C                   eval      vinFLG = '2'
028500060710     c                   eval      esito  = '1'
028600060621     c                   end
028700060612
028800060612     c                   update    Tivin000
028900060614     c                   endIF
029000060612
029100060612     c                   read      Tivin00r
029200060612     C                   ENDdo
029300060612      **
029400060612     c                   endSR
029500060612      * ?------------------------------------------------------------------ */
029600060612      *?    Decodifica record a campi variabili
029700060612      * ?------------------------------------------------------------------ */
029800060612     c     Decod_Record  Begsr
029900060612
030000060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
030100060612     c                   movel(p)  VINDTA        dati
030200060612      **
030300060612      * ?              /*---------------------- */
030400060612     c                   exsr      Split_Rec
030500060612      * ?              /*---------------------- */
030600060612      **
030700060612      **  Records di Intestazione e di fine Msg
030800060612     c                   if        %subst(dati:1:1) ='#'
030900060612      * ?              /*---------------------- */
031000060612     c                   exsr      Rec_Speciali
031100060612      * ?              /*---------------------- */
031200060612     c                   Else
031300060612      ** conta le righe
031400060612      * ?              /*---------------------- */
031500060612     c                   exsr      Rec_Dettaglio
031600060612      * ?              /*---------------------- */
031700060612     c                   End
031800060612      **
031900060612     c                   Endsr
032000060612      * ?------------------------------------------------------------------ */
032100060612      *?      Record di Testata e di Fine Msg
032200060612      * ?------------------------------------------------------------------ */
032300060612     c     Rec_Speciali  Begsr
032400060612      **
032500060612      * ?  Prima riga
032600060612     c     'FILE'        SCAN      dati:2        position
032700060612     c                   if        %Found and position < 5
032800060612     c                   exsr      Rec_UNO
032900060612     c                   ElSe
033000060612     c                   End
033100060612      **
033200060809      * ?  Seconda riga (verisone)
033300060809     c     'Version'     SCAN      dati:2        position
033400060612     c                   if        %Found and position < 5
033500060809     c                   exsr      Rec_DUE
033600060612     c                   End
033700060809      **
033800060809      * ?  Terza riga (testate dei campi)
033900060809     c     'DEF'         SCAN      dati:2        position
034000060809     c                   if        %Found and position < 5
034100060809     c                   exsr      Rec_TRE
034200060809     c                   End
034300060612      **
034400060612      * ?  Ultima riga
034500060612     c     'END'         SCAN      dati:2        position
034600060612     c                   if        %Found and position < 5
034700060612     c                   exsr      Rec_LAST
034800060612     c                   End
034900060612      **
035000060612     c                   Endsr
035100060612      * ?------------------------------------------------------------------ */
035200060612      *?      Primo Record
035300060612      * ?------------------------------------------------------------------ */
035400060612     c     Rec_UNO       Begsr
035500060612      **
035600060612      ** esempio di record :
035700060808      * ?   # FILE;N00;depot945;20050603;143526;6;
035800060612      **
035900060612      **  Inizializza i campi della trasmissione
036000060612     C                   eval      Depot_Trs_UNO = *blank
036100060612     C                   eval      User_Trs_UNO  = *blank
036200060612     C                   eval      Data_Trs_UNO  = 0
036300060612     C                   eval      Time_Trs_UNO  = 0
036400060612     C                   eval      Num_Trs_UNO   = 0
036500060612
036600060612      **   Memorizza il numero della trasm. da confrontare con l'ultimo record.
036700060612      **     Tutto il dettaglio deve essere compreso entro questi 2 records
036800060612      **    altrimenti il msg.non è completo e bisogna farsi rimandare il file.
036900060612
037000060808      * ? Nel 2° campo di testata c'è lo USER in Delis
037100060808     C                   z-add     2             sk
037200060808     c                   eval      User_Trs_UNO = %subst(Dato_sk(sk):1:+
037300060808     c                             LunDato_sk(sk))
037400060808
037500060808      * ? Nel 3° campo di testata c'è il Dispatch Depot
037600060808     C                   z-add     3             sk
037700060808     c                   eval      Depot_Trs_UNO = %subst(Dato_sk(sk):1:+
037800060808     c                             LunDato_sk(sk))
037900060808
038000060808      * ? Nel 4° campo di testata c'è la data trasmissione
038100060808     C                   z-add     4             sk
038200060808     c                   if        DatoNum_sk(sk) = 'S'
038300060808     c                   eval      Data_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
038400060808     c                   End
038500060808
038600060808      * ? Nel 5° campo di testata c'è il nr.seriale di trasmissione
038700060808     C                   z-add     5             sk
038800060808     c                   if        DatoNum_sk(sk) = 'S'
038900060808     c                   eval      Time_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
039000060808     c                   End
039100060808
039200060808      * ? Nel 6° campo di testata c'è il nr.seriale di trasmissione
039300060808     C                   z-add     6             sk
039400060808     c                   if        DatoNum_sk(sk) = 'S'
039500060808     c                   eval      num_trs_UNO = atoi(%TRIM(Dato_sk(sk)))
039600060808     c                   End
039700060612      **
039800060612     c                   Endsr
039900060612      * ?------------------------------------------------------------------ */
040000060612      *?      Secondo Record
040100060612      * ?------------------------------------------------------------------ */
040200060612     c     Rec_DUE       Begsr
040300060809      **
040400060809      ** esempio di record :
040500060809      * ?   # Version;JR20040831;
040600060809      **
040700060809      **  Inizializza i campi della versione trasmissione
040800060809     C                   eval      Vers_Trs_DUE = *blank
040900060809      **
041000060809      **   Memorizza la versione i trasm.
041100060809
041200060809      * ? Nel 2° campo di testata c'è lo USER in Delis
041300060809     C                   z-add     2             sk
041400060809     c                   eval      Vers_Trs_DUE = %subst(Dato_sk(sk):1:+
041500060809     c                             LunDato_sk(sk))
041600060809      **
041700060809     c                   Endsr
041800060809      * ?------------------------------------------------------------------ */
041900060809      *?      Terzo   Record
042000060809      * ?------------------------------------------------------------------ */
042100060809     c     Rec_TRE       Begsr
042200060809      **
042300060612      ** esempio di record :
042400060808      * ?   # DEF;CR:CRDATA01;ODEPOT;ORDERNR;CDEPOT;OCUSTNO;ONAME;ONAME2;OS...
042500060612      **
042600060612      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
042700060612     C                   clear                   Testa_Sk
042800060612
042900060612      * ? Dal 3° campo troviamo i nomi delle intestazioni dei campi C.Request
043000060612     C     3             do        50            sk
043100060612     c                   eval      Testa_Sk(sk-2) = %subst(Dato_sk(sk):1:+
043200060612     c                             LunDato_sk(sk))
043300060612     c                   enddo
043400060612      **
043500060612     c                   Endsr
043600060612      * ?------------------------------------------------------------------ */
043700060612      *?      Ultimo  Record
043800060612      * ?------------------------------------------------------------------ */
043900060612     c     Rec_LAST      Begsr
044000060612      **
044100060612      ** esempio di record :
044200060808      * ?   # END;6;
044300060612      **
044400060612      **  Inizializza i campi della trasmissione
044500060612     C                   eval      Num_Trs_fin = 0
044600060612      **
044700060612      **  Nell'ultimo record di chiusura deve esserci lo stesso numero di trasmissione
044800060612      **  impostato nel primo record.
044900060612      **
045000060612      * ? Nel 2° campo di fine trsm. c'è il nr.seriale di trasmissione
045100060612     C                   z-add     2             sk
045200060612     c                   if        DatoNum_sk(sk) = 'S'
045300060612     c                   eval      num_trs_fin = atoi(%TRIM(Dato_sk(sk)))
045400060612     c                   End
045500060612      **
045600060612     c                   Endsr
045700060612      * ?------------------------------------------------------------------ */
045800060612      *?      Riga Bolla Import da controllare e tradurre
045900060612      * ?------------------------------------------------------------------ */
046000060612     c     Rec_Dettaglio Begsr
046100060612      **
046200060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
046300060612      **
046400060809     c                   clear                   fidpoDTA1
046500060614     c                   clear                   Msg_Err
046600110930     c                   clear                   ONAME_maiu
046700110930     c                   clear                   ONAME2_maiu
046800110930     c                   clear                   OSTREET_maiu
046900110930     c                   clear                   OCITY_maiu
047000110930     c                   clear                   CNAME_maiu
047100110930     c                   clear                   CNAME2_maiu
047200110930     c                   clear                   CNAME3_maiu
047300110930     c                   clear                   CNAME4_maiu
047400110930     c                   clear                   CSTREET_maiu
047500110930     c                   clear                   CCITY_maiu
047600110930     c                   clear                   RNAME_maiu
047700110930     c                   clear                   RNAME2_maiu
047800110930     c                   clear                   RSTREET_maiu
047900110930     c                   clear                   RCITY_maiu
048000110930     c                   clear                   CUSTREF_maiu
048100110930     c                   clear                   INFO1_maiu
048200110930     c                   clear                   INFO2_maiu
048300060808      **
048400060808      * ? Nel 1° campo : Numero Depot Ordinante DPD  (Oblligatorio)  ODEPOT
048500060808      **   parte 1 della chiave ORM DPD                              DEPOT - ORDINANTE
048600060808     C                   z-add     1             sk
048700060809     c                   If        LunDato_sk(sk) =  0
048800060808      * ?-----> Errore
048900060809     C                   eval      tipo_error = 1
049000060809     C                   exsr      Field_Error
049100060808     c                   Else
049200060809     c                   eval      ODEPOT   = %subst(Dato_sk(sk):1:+
049300060809     c                                              LunDato_sk(sk))
049400060808     c                   End
049500060808      **
049600060808      * ? Nel 2° campo : Numero Ordine DPD           (Oblligatorio)  ORDERNR
049700060808      **   parte 2 della chiave ORM DPD                              NUM.ORDINE
049800060808     C                   add       1             sk
049900060808     c                   If        DatoNum_sk(sk) = 'N' or
050000060808     c                             LunDato_sk(sk) =  0
050100060808      * ?-----> Errore
050200060809     C                   eval      tipo_error = 2
050300060809     C                   exsr      Field_Error
050400060808     c                   Else
050500060808     c                   eval      ORDERNR  = atoi(%TRIM(Dato_sk(sk)))
050600060808     c                   End
050700060808      **
050800060808      * ? Nel 3° campo : Depot Che deve eseguire ORM                 CDEPOT
050900060808      **                                                             DEPOT DI RITIRO
051000060808     C                   add       1             sk
051100060809     c                   If        LunDato_sk(sk) =  0
051200060808      * ?-----> Errore
051300060809     C                   eval      tipo_error = 1
051400060809     C                   exsr      Field_Error
051500060808     c                   Else
051600060809     c                   eval      CDEPOT   = %subst(Dato_sk(sk):1:+
051700060809     c                                              LunDato_sk(sk))
051800060808     c                   End
051900060808      **
052000060808      * ? Nel 4° campo : Codice Cliente Ordinante                    OCUSTNO
052100060808      **                                                             CLIENTE ORDINANTE
052200060808     C                   add       1             sk
052300060809     c                   If        DatoNum_sk(sk) = 'N' or
052400060809     c                             LunDato_sk(sk) =  0
052500060808      * ?-----> Errore
052600060809     C                   eval      tipo_error = 2
052700060809     C                   exsr      Field_Error
052800060808     c                   Else
052900060808     c                   eval      OCUSTNO  = atoi(%TRIM(Dato_sk(sk)))
053000060808     c                   End
053100060808      **
053200060808      * ? Nel 5° campo : Nome Cliente Ordinante      (Oblligatorio)  ONAME
053300060808      **                                                             NOME ORDINANTE
053400060808     C                   add       1             sk
053500060808     c                   If        LunDato_sk(sk) =  0
053600060808      * ?-----> Errore
053700060809     C                   eval      tipo_error = 1
053800060809     C                   exsr      Field_Error
053900060808     c                   Else
054000060808     c                   eval      ONAME    = %subst(Dato_sk(sk):1:+
054100060808     c                                              LunDato_sk(sk))
054200061002     c     minu:maiu     XLATE     ONAME         ONAME_maiu                     *Minus -> Maiuscolo
054300060808     c                   End
054400060808      **
054500060808      * ? Nel 6° campo : 2° Nome Cliente Ordinante                   ONAME2
054600060808      **                                                             NOME 2 ORDINANTE
054700060808     C                   add       1             sk
054800060808     c                   If        LunDato_sk(sk) >  0
054900060808     c                   eval      ONAME2   = %subst(Dato_sk(sk):1:+
055000060808     c                                              LunDato_sk(sk))
055100061002     c     minu:maiu     XLATE     ONAME2        ONAME2_maiu                    *Minus -> Maiuscolo
055200060808     c                   End
055300060808      **
055400060808      * ? Nel 7° campo : Indirizzo Ordinante         (Oblligatorio)  OSTREET
055500060808      **                                                             INDIRIZZO ORDINANTE
055600060808     C                   add       1             sk
055700060808     c                   If        LunDato_sk(sk) =  0
055800060808      * ?-----> Errore
055900060809     C                   eval      tipo_error = 1
056000060809     C                   exsr      Field_Error
056100060808     c                   Else
056200060808     c                   eval      OSTREET  = %subst(Dato_sk(sk):1:+
056300060808     c                                              LunDato_sk(sk))
056400061002     c     minu:maiu     XLATE     OSTREET       OSTREET_maiu                   *Minus -> Maiuscolo
056500060808     c                   End
056600060808      **
056700060808      * ? Nel 8° campo : Nazione Ordinante           (Oblligatorio)  OCOUNTRY
056800060808      **                                                             NAZIONE ORDINANTE
056900060808     C                   add       1             sk
057000060809     c                   If        DatoNum_sk(sk) = 'N' or
057100060809     c                             LunDato_sk(sk) =  0
057200060809      * ?-----> Errore
057300060809     C                   eval      tipo_error = 2
057400060809     C                   exsr      Field_Error
057500060808     c                   Else
057600060809     c                   eval      OCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
057700060808     c                   End
057800060808      **
057900060808      * ? Nel 9° campo : Cod.Postale Ordinante       (Oblligatorio)  OPOSTAL
058000060808      **                                                             CAP ORDINANTE
058100060808     C                   add       1             sk
058200060808     c                   If        LunDato_sk(sk) =  0
058300060808      * ?-----> Errore
058400060809     C                   eval      tipo_error = 1
058500060809     C                   exsr      Field_Error
058600060808     c                   Else
058700060808     c                   eval      OPOSTAL  = %subst(Dato_sk(sk):1:+
058800060808     c                                              LunDato_sk(sk))
058900060808     c                   End
059000060808      **
059100060808      * ? Nel 10° campo : Città Ordinante            (Oblligatorio)  OCITY
059200060808      **                                                             CITTÀ ORDINANTE
059300060808     C                   add       1             sk
059400060808     c                   If        LunDato_sk(sk) =  0
059500060808      * ?-----> Errore
059600060809     C                   eval      tipo_error = 1
059700060809     C                   exsr      Field_Error
059800060808     c                   Else
059900060808     c                   eval      OCITY    = %subst(Dato_sk(sk):1:+
060000060808     c                                              LunDato_sk(sk))
060100061002     c     minu:maiu     XLATE     OCITY         OCITY_maiu                     *Minus -> Maiuscolo
060200060808     c                   End
060300060808      **
060400060808      * ? Nel 11° campo : Telefono Ordinante                         OPHONE
060500060808      **                                                             TELEF ORDINANTE
060600060808     C                   add       1             sk
060700060808     c                   If        LunDato_sk(sk) >  0
060800060808     c                   eval      OPHONE   = %subst(Dato_sk(sk):1:+
060900060808     c                                              LunDato_sk(sk))
061000060808     c                   End
061100060808      **
061200060808      * ? Nel 12° campo : E-mail   Ordinante                         OEMAIL
061300060808      **                                                             EMAIL ORDINANTE
061400060808     C                   add       1             sk
061500060808     c                   If        LunDato_sk(sk) >  0
061600060808     c                   eval      OEMAIL   = %subst(Dato_sk(sk):1:+
061700060808     c                                              LunDato_sk(sk))
061800060808     c                   End
061900060808      **
062000060808      * ? Nel 13° campo : Nome di Ritiro             (Oblligatorio)  CNAME
062100060808      **                                                             NOME DI RITIRO
062200060808     C                   add       1             sk
062300060808     c                   If        LunDato_sk(sk) =  0
062400060808      * ?-----> Errore
062500060809     C                   eval      tipo_error = 1
062600060809     C                   exsr      Field_Error
062700060808     c                   Else
062800060808     c                   eval      CNAME    = %subst(Dato_sk(sk):1:+
062900060808     c                                              LunDato_sk(sk))
063000061002     c     minu:maiu     XLATE     CNAME         CNAME_maiu                     *Minus -> Maiuscolo
063100060808     c                   End
063200060808      **
063300060808      * ? Nel 14° campo : 2° Nome di Ritiro                          CNAME2
063400060808      **                                                             NOME 2 DI RITIRO
063500060808     C                   add       1             sk
063600060808     c                   If        LunDato_sk(sk) >  0
063700060808     c                   eval      CNAME2   = %subst(Dato_sk(sk):1:+
063800060808     c                                              LunDato_sk(sk))
063900061002     c     minu:maiu     XLATE     CNAME2        CNAME2_maiu                    *Minus -> Maiuscolo
064000060808     c                   End
064100060808      **
064200060808      * ? Nel 15° campo : 3° Nome di Ritiro                          CNAME3
064300060808      **                                                             NOME 3 DI RITIRO
064400060808     C                   add       1             sk
064500060808     c                   If        LunDato_sk(sk) >  0
064600060808     c                   eval      CNAME3   = %subst(Dato_sk(sk):1:+
064700060808     c                                              LunDato_sk(sk))
064800061002     c     minu:maiu     XLATE     CNAME3        CNAME3_maiu                    *Minus -> Maiuscolo
064900060808     c                   End
065000060808      **
065100060808      * ? Nel 16° campo : 4° Nome di Ritiro                          CNAME4
065200060808      **                                                             NOME 4 DI RITIRO
065300060808     C                   add       1             sk
065400060808     c                   If        LunDato_sk(sk) >  0
065500060808     c                   eval      CNAME4   = %subst(Dato_sk(sk):1:+
065600060808     c                                              LunDato_sk(sk))
065700061002     c     minu:maiu     XLATE     CNAME4        CNAME4_maiu                    *Minus -> Maiuscolo
065800060808     c                   End
065900060808      **
066000060808      * ? Nel 17° campo : Indirizzo di Ritiro        (Oblligatorio)  CSTREET
066100060808      **                                                             INDIRIZZO DI RITIRO
066200060808     C                   add       1             sk
066300060808     c                   If        LunDato_sk(sk) =  0
066400060808      * ?-----> Errore
066500060809     C                   eval      tipo_error = 1
066600060809     C                   exsr      Field_Error
066700060808     c                   Else
066800060808     c                   eval      CSTREET  = %subst(Dato_sk(sk):1:+
066900060808     c                                              LunDato_sk(sk))
067000061002     c     minu:maiu     XLATE     CSTREET       CSTREET_maiu                   *Minus -> Maiuscolo
067100060808     c                   End
067200060808      **
067300060808      * ? Nel 18° campo : Nazione di Ritiro          (Oblligatorio)  CCOUNTRY
067400060808      **                                                             NAZIONE DI RITIRO
067500060808     C                   add       1             sk
067600060809     c                   If        DatoNum_sk(sk) = 'N' or
067700060809     c                             LunDato_sk(sk) =  0
067800060808      * ?-----> Errore
067900060809     C                   eval      tipo_error = 2
068000060809     C                   exsr      Field_Error
068100060808     c                   Else
068200060809     c                   eval      CCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
068300060808     c                   End
068400060808      **
068500060808      * ? Nel 19° campo : Cod.Postale di Ritiro      (Oblligatorio)  CPOSTAL
068600060808      **                                                             CAP DI RITIRO
068700060808     C                   add       1             sk
068800060808     c                   If        LunDato_sk(sk) =  0
068900060808      * ?-----> Errore
069000060809     C                   eval      tipo_error = 1
069100060809     C                   exsr      Field_Error
069200060808     c                   Else
069300060808     c                   eval      CPOSTAL  = %subst(Dato_sk(sk):1:+
069400060808     c                                              LunDato_sk(sk))
069500060808     c                   End
069600060808      **
069700060808      * ? Nel 20° campo : Città di Ritiro            (Oblligatorio)  CCITY
069800060808      **                                                             CITTÀ DI RITIRO
069900060808     C                   add       1             sk
070000060808     c                   If        LunDato_sk(sk) =  0
070100060808      * ?-----> Errore
070200060809     C                   eval      tipo_error = 1
070300060809     C                   exsr      Field_Error
070400060808     c                   Else
070500060808     c                   eval      CCITY    = %subst(Dato_sk(sk):1:+
070600060808     c                                              LunDato_sk(sk))
070700061002     c     minu:maiu     XLATE     CCITY         CCITY_maiu                     *Minus -> Maiuscolo
070800060808     c                   End
070900060808      **
071000060808      * ? Nel 21° campo : Telefono di Ritiro                         CPHONE
071100060808      **                                                             TELEF DI RITIRO
071200060808     C                   add       1             sk
071300060808     c                   If        LunDato_sk(sk) >  0
071400060808     c                   eval      CPHONE   = %subst(Dato_sk(sk):1:+
071500060808     c                                              LunDato_sk(sk))
071600060808     c                   End
071700060808      **
071800060808      * ? Nel 22° campo : E-mail   di Ritiro                         CEMAIL
071900060808      **                                                             EMAIL ORDINANTE
072000060808     C                   add       1             sk
072100060808     c                   If        LunDato_sk(sk) >  0
072200060808     c                   eval      CEMAIL   = %subst(Dato_sk(sk):1:+
072300060808     c                                              LunDato_sk(sk))
072400060808     c                   End
072500060808      **
072600060808      * ? Nel 23° campo : Nome di Destino            (Oblligatorio)  RNAME
072700060808      **                                                             NOME DI DESTINO
072800060808     C                   add       1             sk
072900060808     c                   If        LunDato_sk(sk) =  0
073000060808      * ?-----> Errore
073100060809     C                   eval      tipo_error = 1
073200060809     C                   exsr      Field_Error
073300060808     c                   Else
073400060808     c                   eval      RNAME    = %subst(Dato_sk(sk):1:+
073500060808     c                                              LunDato_sk(sk))
073600061002     c     minu:maiu     XLATE     RNAME         RNAME_maiu                     *Minus -> Maiuscolo
073700060808     c                   End
073800060808      **
073900060808      * ? Nel 24° campo : 2° Nome di Destino                         RNAME2
074000060808      **                                                             NOME 2 DI DESTINO
074100060808     C                   add       1             sk
074200060808     c                   If        LunDato_sk(sk) >  0
074300060808     c                   eval      RNAME2   = %subst(Dato_sk(sk):1:+
074400060808     c                                              LunDato_sk(sk))
074500061002     c     minu:maiu     XLATE     RNAME2        RNAME2_maiu                    *Minus -> Maiuscolo
074600060808     c                   End
074700060808      **
074800060808      * ? Nel 25° campo : Indirizzo di Destino       (Oblligatorio)  RSTREET
074900060808      **                                                             INDIRIZZO DI DESTINO
075000060808     C                   add       1             sk
075100060808     c                   If        LunDato_sk(sk) =  0
075200060808      * ?-----> Errore
075300060809     C                   eval      tipo_error = 1
075400060809     C                   exsr      Field_Error
075500060808     c                   Else
075600060808     c                   eval      RSTREET  = %subst(Dato_sk(sk):1:+
075700060808     c                                              LunDato_sk(sk))
075800061002     c     minu:maiu     XLATE     RSTREET       RSTREET_maiu                   *Minus -> Maiuscolo
075900060808     c                   End
076000060808      **
076100060808      * ? Nel 26° campo : Nazione di Destino         (Oblligatorio)  RCOUNTRY
076200060808      **                                                             NAZIONE DI DESTINO
076300060808     C                   add       1             sk
076400060809     c                   If        DatoNum_sk(sk) = 'N' or
076500060809     c                             LunDato_sk(sk) =  0
076600060808      * ?-----> Errore
076700060809     C                   eval      tipo_error = 2
076800060809     C                   exsr      Field_Error
076900060808     c                   Else
077000060809     c                   eval      RCOUNTRY = atoi(%TRIM(Dato_sk(sk)))
077100060808     c                   End
077200060808      **
077300060808      * ? Nel 27° campo : Cod.Postale di Destino     (Oblligatorio)  RPOSTAL
077400060808      **                                                             CAP DI DESTINO
077500060808     C                   add       1             sk
077600060808     c                   If        LunDato_sk(sk) =  0
077700060808      * ?-----> Errore
077800060809     C                   eval      tipo_error = 1
077900060809     C                   exsr      Field_Error
078000060808     c                   Else
078100060808     c                   eval      RPOSTAL  = %subst(Dato_sk(sk):1:+
078200060808     c                                              LunDato_sk(sk))
078300060808     c                   End
078400060808      **
078500060808      * ? Nel 28° campo : Città di Destino           (Oblligatorio)  RCITY
078600060808      **                                                             CITTÀ DI DESTINO
078700060808     C                   add       1             sk
078800060808     c                   If        LunDato_sk(sk) =  0
078900060808      * ?-----> Errore
079000060809     C                   eval      tipo_error = 1
079100060809     C                   exsr      Field_Error
079200060808     c                   Else
079300060808     c                   eval      RCITY    = %subst(Dato_sk(sk):1:+
079400060808     c                                              LunDato_sk(sk))
079500061002     c     minu:maiu     XLATE     RCITY         RCITY_maiu                     *Minus -> Maiuscolo
079600060808     c                   End
079700060808      **
079800060808      * ? Nel 29° campo : Telefono di Destino                        RPHONE
079900060808      **                                                             TELEF DI DESTINO
080000060808     C                   add       1             sk
080100060808     c                   If        LunDato_sk(sk) >  0
080200060808     c                   eval      RPHONE   = %subst(Dato_sk(sk):1:+
080300060808     c                                              LunDato_sk(sk))
080400060808     c                   End
080500060808      **
080600060808      * ? Nel 30° campo : E-mail   di Destino                        REMAIL
080700060808      **                                                             EMAIL DI DESTINO
080800060808     C                   add       1             sk
080900060808     c                   If        LunDato_sk(sk) >  0
081000060808     c                   eval      REMAIL   = %subst(Dato_sk(sk):1:+
081100060808     c                                              LunDato_sk(sk))
081200060808     c                   End
081300060808      **
081400060808      * ? Nel 31° campo : Depot di Destino                           RDEPOT
081500060808      **                                                             DEPOT DI DESTINO
081600060808     C                   add       1             sk
081700060809     c                   If        LunDato_sk(sk) >  0
081800060809     c                   eval      RDEPOT   = %subst(Dato_sk(sk):1:+
081900060809     c                                              LunDato_sk(sk))
082000060808     c                   End
082100060808      **
082200060808      * ? Nel 32° campo : Parcel Number                              PKNR
082300060808      **                                                             PARCEL NR
082400060808     C                   add       1             sk
082500060808     c                   If        LunDato_sk(sk) >  0
082600060808     c                   eval      PKNR     = %subst(Dato_sk(sk):1:+
082700060808     c                                              LunDato_sk(sk))
082800060809      * ?   ( Il parcel non è + numerico )   controlla lunghezza del Parcel
082900060809     c                   clear                   Lung_Parcel
083000060809     c                   eval      Lung_Parcel = %Len(%trim(PKNR))
083100060809     c                   if        Lung_Parcel = 11
083200060809     C                   MOVEL     PKNR          parcel11
083300060809     c                   end
083400060809     c                   if        Lung_Parcel = 14
083500060809     C                   MOVEL     PKNR          parcel14
083600060809     c                   end
083700060809      **
083800060808     c                   End
083900060808      **
084000060808      * ? Nel 33° campo : Status Digit                               STATUS
084100060808      **                                                             STATUS
084200060808     C                   add       1             sk
084300060808     c                   If        LunDato_sk(sk) >  0
084400060808     c                   eval      STATUS   = %subst(Dato_sk(sk):1:+
084500060808     c                                              LunDato_sk(sk))
084600060808     c                   End
084700060808      **
084800060808      * ? Nel 34° campo : Cancellation Digit                         IDENT
084900060808      **                                                             CANCELLATO
085000060808     C                   add       1             sk
085100060808     c                   If        LunDato_sk(sk) >  0
085200060808     c                   eval      IDENT    = %subst(Dato_sk(sk):1:+
085300060808     c                                              LunDato_sk(sk))
085400060808     c                   End
085500060808      **
085600060808      * ? Nel 35° campo : Riferimento Cliente                        CUSTREF
085700060808      **                                                             RIFERIMENTO CLIENTE
085800060808     C                   add       1             sk
085900060808     c                   If        LunDato_sk(sk) >  0
086000060808     c                   eval      CUSTREF  = %subst(Dato_sk(sk):1:+
086100060808     c                                              LunDato_sk(sk))
086200061002     c     minu:maiu     XLATE     CUSTREF       CUSTREF_maiu                   *Minus -> Maiuscolo
086300060808     c                   End
086400060808      **
086500060808      * ? Nel 36° campo : Numero Ordine WEB                          WEBNR
086600060808      **                                                             ORDINE WEB
086700060808     C                   add       1             sk
086800060808     c                   If        LunDato_sk(sk) >  0
086900060808     c                   eval      WEBNR    = %subst(Dato_sk(sk):1:+
087000060808     c                                              LunDato_sk(sk))
087100060808     c                   End
087200060808      **
087300060808      * ? Nel 37° campo : Data Creazione Ordine                      NEUDAT
087400060808      **                                                             DATA CREAZIONE ORDINE
087500060808     C                   add       1             sk
087600060808     c                   If        DatoNum_sk(sk) = 'N' and
087700060808     c                             LunDato_sk(sk) >  0
087800060808      * ?-----> Errore
087900060809     C                   eval      tipo_error = 3
088000060809     C                   exsr      Field_Error
088100060808     c                   Else
088200060808     c                   eval      NEUDAT   = atoi(%TRIM(Dato_sk(sk)))
088300060808     c                   End
088400060808      **
088500060808      * ? Nel 38° campo : Data Generazione                           AENDDAT
088600060808      **                                                             DATA GENERAZIONE
088700060808     C                   add       1             sk
088800060808     c                   If        DatoNum_sk(sk) = 'N' and
088900060808     c                             LunDato_sk(sk) >  0
089000060808      * ?-----> Errore
089100060809     C                   eval      tipo_error = 3
089200060809     C                   exsr      Field_Error
089300060808     c                   Else
089400060808     c                   eval      AENDDAT  = atoi(%TRIM(Dato_sk(sk)))
089500060808     c                   End
089600060809      **
089700060809      * ? Nel 39° campo : SERVICE CODE                               SERVICE
089800060809      **                                                             SERVICE CODE
089900060809     C                   add       1             sk
090000060809     c                   If        DatoNum_sk(sk) = 'N' and
090100060809     c                             LunDato_sk(sk) >  0
090200060809      * ?-----> Errore
090300060809     C                   eval      tipo_error = 3
090400060809     C                   exsr      Field_Error
090500060809     c                   Else
090600060809     c                   eval      SERVICE  = atoi(%TRIM(Dato_sk(sk)))
090700060809     c                   End
090800060809      **
090900060809      * ? Nel 40° campo : CUSTOM  status code                        CUSTOM
091000060809      **                                                             CUSTOM S.CODE
091100060809     C                   add       1             sk
091200060809     c                   If        LunDato_sk(sk) >  0
091300060809      * ?-----> Errore
091400060809     c                   eval      CUSTOM   = %subst(Dato_sk(sk):1:+
091500060809     c                                              LunDato_sk(sk))
091600060809     c                   End
091700060808      **
091800060809      * ? Nel 41° campo : Informazione 1                             INFO1
091900060808      **                                                             INFORMAZIONE 1
092000060808     C                   add       1             sk
092100060808     c                   If        LunDato_sk(sk) >  0
092200060808     c                   eval      INFO1    = %subst(Dato_sk(sk):1:+
092300060808     c                                              LunDato_sk(sk))
092400061002     c     minu:maiu     XLATE     INFO1         INFO1_maiu                     *Minus -> Maiuscolo
092500060808     c                   End
092600060808      **
092700060809      * ? Nel 42° campo : Informazione 2                             INFO2
092800060808      **                                                             INFORMAZIONE 2
092900060808     C                   add       1             sk
093000060808     c                   If        LunDato_sk(sk) >  0
093100060808     c                   eval      INFO2    = %subst(Dato_sk(sk):1:+
093200060808     c                                              LunDato_sk(sk))
093300061002     c     minu:maiu     XLATE     INFO2         INFO2_maiu                     *Minus -> Maiuscolo
093400060808     c                   End
093500060808      **
093600060809      * ? Nel 43° campo : Peso (Con decimali)                        WEIGHT
093700060808      **                                                             PESO IN KG
093800060808     C                   add       1             sk
093900060808     c                   If        DatoNum_sk(sk) = 'N' and
094000060808     c                             LunDato_sk(sk) >  0
094100060808      * ?-----> Errore
094200060809     C                   eval      tipo_error = 3
094300060809     C                   exsr      Field_Error
094400060808     c                   Else
094500060808     c                   eval      WEIGHT   = atof(%TRIM(Dato_sk(sk)))
094600060808     c                   End
094700060808      **
094800060809      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL FIDPODTA1
094900060612      **
095000060612     c                   Endsr
095100060614      * ?------------------------------------------------------------------ */
095200060614      *?      Campi in errore
095300060614      * ?------------------------------------------------------------------ */
095400060614     c     Field_Error   Begsr
095500060614      **
095600060614     C                   eval      se_errore   = 'S'
095700060614      * msg video o in posta
095800060614     c                   If        Msg_Err = *blank
095900060614     c
096000060614     c                   Select
096100060614     c
096200060614     c                   When      Tipo_Error = 1
096300060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
096400060614     C                             ' >> Obblig.- Manca Dato !!'
096500060614     c
096600060614     c                   When      Tipo_Error = 2
096700060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
096800060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
096900060614     c
097000060614     c                   When      Tipo_Error = 3
097100060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
097200060614     C                             ' >> NON Numerico  !!'
097300060614     c
097400060614     c                   When      Tipo_Error = 4
097500060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
097600060614     C                             ' >> Conversione non riuscita !!'
097700060614     c                   EndSL
097800060614     c
097900060614     c                   End
098000060614      * Errore su VINMSG
098100060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
098200060614     c                                          + %trim(Testa_sk(sk))
098300060614      **
098400060614     c                   Endsr
098500060808      * ?------------------------------------------------------------------ */
098600060808      *?    Scrive i Files degli ORM
098700060808      * ?------------------------------------------------------------------ */
098800060809     c     WRT_FILE      Begsr
098900060808      **
099000060808      **  se richiesta cancellazione di un Collection Request precedente
099100060808     c                   if        IDENT = '3'
099200060808      **?              /*---------------------- */
099300060808     c                   ExSr      STORNO
099400060808      * ?              /*---------------------- */
099500060808     c                   else
099600080430      **
099700080430      **  controlla sul FIDPO se era già arrivato il depot/nr.ordine x errore DPD
099800080430      **  o se si stanno reintroducendo erroneamente gli stessi dati.
099900080430      **   ATTENZIONE !!!!
100000080430     C                   eval      DP3DEP   = ODEPOT
100100080430     C                   eval      DP3ORD   = ORDERNR
100200080430     c     dp3_Ordine    setll     Fidpo03l
100300080430      **
100400080430      **?              /*- se non c'è è OK   -- */
100500080430     c                   if        not %equal
100600080430      **
100700060808      **  Se è in questo punto significa che non sono stati trovati errori
100800080430      **    formali quindi si passa alla scrittura
100900080430      **   e del FIDPO00F con solo l'Ordine DPD impostato
101000080430      **?              /*---------------------- */
101100080430     c                   ExSr      Crea_FIDPO
101200080430      * ?              /*---------------------- */
101300080430      **   e dell'FNVAO000 per impostare l'ORM dal Collection Request
101400060808      **?              /*---------------------- */
101500060808     c                   ExSr      Crea_VAO
101600060808      * ?              /*---------------------- */
101700080430      *
101800080430     c                   else
101900080430      **
102000080430      **?              /*-  altrimenti MSG errore */
102100080430     c                   eval      se_errore ='S'
102200080430     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)
102300080430     c                               + '; ordine già ricevuto da DPD'
102400080430      **
102500080430     c                   end
102600080430      *
102700060808     c                   end
102800060809
102900060809     c                   endSR
103000060808      * ?------------------------------------------------------------------ */
103100060808      *?    Imposta i campi per il VAO
103200060808      * ?------------------------------------------------------------------ */
103300060808     c     Crea_FIDPO    Begsr
103400060808      **
103500060808     c                   clear                   fidpo000
103600060808     C* ?Nr.ORM  DPD
103700060808     C                   eval      DPODEP   = ODEPOT
103800060808     C                   eval      DPOORD   = ORDERNR
103900060808     C                   eval      DPODTRIC = data_oggi
104000080718      *
104100080718     C* ?Nei primi 4 bytes del Filler impostiamo il DEPOT di RITIRO
104200080718     C* ?Importante x DPD quando restituiamo il CRDATA02 :
104300080718     C* ?Deve essere ASSOLUTAMENTE identico nel CRDATA02 il CDEPOT
104400080718     C* ?altrimenti DPD non riesce a dare le informazioni correttamente al
104500080718     C* ?Depot che ha ordinato il ritiro... quindi dirgli qual'è il PARCEL
104600080718     C* ?che è stato ritirato e che arriverà in consegna e quindi che potrà
104700080718     C* ?vedere nell'Iter del Track & Trace.
104800080718     c                   movel     CDEPOT        dpoFLR
104900080718     C* ?quindi attenzione al Filler
105000080718      *
105100060808     c                   write     fidpo000
105200060808      **
105300060808     c                   Endsr
105400060808      * ?------------------------------------------------------------------ */
105500060808      *?    Imposta i campi per il VAO
105600060808      * ?------------------------------------------------------------------ */
105700060808     c     Crea_VAO      Begsr
105800060808      **
105900060808     c                   clear                   fnvao000
106000060808     c                   clear                   err_bloccante     1
106100060914      **
106200060914      ** tipo comunicazione ORM
106300060914     C                   eval      vaotco = 'F'
106400060808      **
106500061002     C                   eval      vaorsr = CNAME_maiu
106600061002     C                   eval      vaoinr = CSTREET_maiu
106700060808     C                   eval      vaocar = CPOSTAL
106800061002     C                   eval      vaolor = CCITY_maiu
106900061129     C                   eval      vaoter = CPHONE
107000061129     C                   eval      vaorer = CNAME2_maiu
107100061006     C*
107200061006     C* CAP OBBLIGATORIO : CONTROLLO
107300061006     C                   CLEAR                   DSSI95
107400061006     C                   CLEAR                   DSLV13
107500061006     C                   MOVEL     '3'           I95TCN
107600061006     C                   MOVEL     vaocar        I95CAP
107700061006     C                   MOVEL     vaolor        I95LOC
107800061006     C* ERRORE AFFIDABILITA = 0
107900061006     C                   MOVEL     'S'           I13AF0
108000061006     C                   MOVEL     'S'           I13AF1
108100080430     C                   z-add     1             E13FZ1
108200061006     C                   CALL      'FNLV13R'
108300061006     C                   PARM                    KPJBA
108400061006     C                   PARM                    DSLV13
108500061006     C                   PARM                    DSSI95
108600061006     C* ERRORE
108700061006     C     O13ERR        IFEQ      *BLANKS
108800061006     C                   MOVEL     O95PRV        vaoPRR
108900061006    3C                   ENDIF
109000061006     C*
109100060808     C* Nazione dove effettuare il Ritiro
109200060808     C                   z-add     1             Nz
109300061002     C     cCountry      lookup    Cie03(Nz)                              99
109400080404DO  1C                   if        %Equal
109500060808     C                   MOVEL     Naz03(Nz)     vaoNAR
109600060808E   1C                   End
109700060808      *
109800061002     C                   eval      vaorsc = RNAME_maiu
109900061002     C                   eval      vaoinc = RSTREET_maiu
110000060808     C                   eval      vaocac = RPOSTAL
110100061002     C                   eval      vaoloc = RCITY_maiu
110200061006     C*
110300081031     C*  SE IL DESTINATARIO È EXAPAQ località "LIEUSAINT CEDEX" cap fittizio 00036
110400081031     C*   si deve sostituire il cap con "77564" CAP reale .
110500081031     C*  Richiesta di DPD da parte della Villa confermato con mail del 31/10/2008.
110600081031     C*
110700081031     C*  indirizzo destinatario:
110800081031     C*    DEposito 1003.
110900081031     C*    EXAPAQ SA
111000081031     C*    BOULEVARD DU LUXEMBOURG ZAC PARISUD
111100081031     C*    (cap fittizio 00036) cap vero 77564     LIEUSAINT CEDEX         F
111200081031     C*                                 =======
111300081031     c                   clear                   LieuSced         15
111400081031     C                   eval      LieuSced = 'LIEUSAINT CEDEX'
111500081031     c     LieuSced      scan      vaoloc                                 99
111600081031     C                   If        *in99
111700081031     c     '00036'       scan      vaocac                                 99
111800081031     C                   If        *in99
111900081031     c                   eVAl      vaoCAC = '77564'
112000081031     C                   EnD
112100081031     C                   EnD
112200081031     C*
112300061006     C* CAP OBBLIGATORIO : CONTROLLO
112400061006     C                   CLEAR                   DSSI95
112500061006     C                   CLEAR                   DSLV13
112600061006     C                   MOVEL     '3'           I95TCN
112700061006     C                   MOVEL     vaocac        I95CAP
112800061006     C                   MOVEL     vaoloc        I95LOC
112900061006     C* ERRORE AFFIDABILITA = 0
113000061006     C                   MOVEL     'S'           I13AF0
113100061006     C                   MOVEL     'S'           I13AF1
113200080430     C                   z-add     1             E13FZ1
113300061006     C                   CALL      'FNLV13R'
113400061006     C                   PARM                    KPJBA
113500061006     C                   PARM                    DSLV13
113600061006     C                   PARM                    DSSI95
113700061006     C* ERRORE
113800061006     C     O13ERR        IFEQ      *BLANKS
113900061006     C                   MOVEL     O95PRV        vaoPRc
114000061006    3C                   ENDIF
114100061006     C*
114200060808     C* Nazione di Consegna
114300060808     C                   z-add     1             Nz
114400061002     C     rCountry      lookup    Cie03(Nz)                              99
114500080404DO  1C                   if        %Equal
114600060808     C                   MOVEL     Naz03(Nz)     vaoNAC
114700060808E   1C                   End
114800060808      *
114900060808     C                   eval      vaodao = Data_Trs_UNO
115000060808     C                   eval      vaooao = Time_Trs_UNO
115100061002     C                   eval      vaono1 = INFO1_maiu
115200061002     C                   eval      vaono2 = INFO2_maiu
115300120528      **
115400120528      *** Su richiesta di Christian Burke: per quanto riguarda il referente di ritiro
115500120528      **   viene spesso impostato sul loro campo INFO1 e quindi la richiesta è di
115600120528      **   riportarlo nel nostro file come referente.
115700120528     C                   eval      vaorer = %trim(vaorer) +' '+ INFO1_maiu
115800120528     C                   eval      vaorer = %trim(vaorer) +' '+ INFO2_maiu
115900120528      ***
116000120528      **
116100060808     C                   eval      vaopkg = WEIGHT
116200080512      * se non ricevuto il peso imposta 11kg.
116300080512     c                   if        vaopkg = 0
116400080512     c                   z-add     11            vaopkg
116500080512     c                   end
116600080512      * imposta fisso le 12:00 come ora di ritiro
116700080512     c                   z-add     1200          vaoorr
116800060808     C                   eval      vaoncl = 1
116900060808     C                   eval      vaotor = 'S'
117000060808     C*-------------------------
117100080527     C* ?Si era deciso che gli ORM dovevano essere presi tutti con linea 190
117200080527     C                   z-add     190           vaoPOE
117300080527     C                   z-add     190           vaoPOC
117400140930      **----
117500140929      **  Aggancia la LINEA dal "Depot ORDINANTE" e se NON riesce segue la vecchia
117600140929      **  strada della NAZ ORDinante (eccezione x l'Italia)
117700140930     c                   z-add     0             in_POE            3 0
117800140930      *
117900140929      **               *-------------------------*
118000140929     c                   exsr      NAZ_da_oDepot
118100140929      **               *-------------------------*
118200140930      *
118300140930      ** Se ha trovato la linea
118400140930      **  dalla Routine dei files del Cappario imposta questa sul POE
118500140930     c                   if        in_POE > 0
118600140930     C                   z-add     in_POE        vaoPOE
118700140930      **  altrimenti esegue le vecchie specifiche
118800140930      **   con cui impostare il codice POE
118900140930     c                   else
119000140929      **
119100080708     C*   in base alla Nazione del Depot Ordinante ricava Torino/Verona
119200060808     C                   z-add     1             Nz
119300140924      ** solo se l'ordinante è Italia, con la Nazione di Destino cerca di
119400140924      **  decodificare la corretta linea di partenza
119500140924      **  che normalmente deve essere sempre decodificata con la Naz.Ordinante
119600140924     c                   if        oCountry = 380
119700140924     C     rCountry      lookup    Cie03(Nz)                              99
119800140924     c                   else
119900140924      *-
120000140924      *- normalmente deve cercare con Ocountry.
120100080708     C     oCountry      lookup    Cie03(Nz)                              99
120200140924      *-
120300140924E   1C                   End
120400140924      *-
120500080708DO  1C                   if        %Equal and POE03(Nz) > 0
120600080604     C                   MOVEL     POE03(Nz)     vaoPOE
120700080527E   1C                   End
120800080708      *
120900140930     c                   endIF
121000140930      **----
121100080708      **  se Italia la destinazione di Consegna non imposto il P.O.di Consegna
121200080708     c                   if        rCountry = 380
121300080708     C                   z-add     0             vaoPOC
121400080708     c                   else
121500080708      **
121600080708     C*   in base alla Nazione di Consegna ->linea DPD x P.O. di Consegna
121700080708     C                   z-add     1             Nz
121800080708     C     rCountry      lookup    Cie03(Nz)                              99
121900080708     c                   if        %Equal and DpD03(Nz) > 0
122000080527     C                   MOVEL     DpD03(Nz)     vaoPOC
122100080527E   1C                   End
122200080708      **
122300080708     c                   end
122400080521      **
122500060808     C*--------------------
122600060808     C* ?Nr.ORM  DPD su riferimento Alfanumerico
122700060809     c                   move      ODEPOT        ordDep4           4
122800060808     c                   move      ORDERNR       ordNum6           6
122900060811     C                   eval      VAORFA = 'DPD' + ordDep4 + ordNum6
123000060808     C*--------------------
123100060808     C                   eval      VAOPAG = 'O'
123200060808     C* ?Codice Ordinante
123300060925     C* ? reperisce il codice cliente in base al depot ordinante
123400060925     C* ? e se non lo trova cerca di impostarlo con il default sulla PT
123500060925     C* ? per il codice DPD190
123600060925     C                   clear                   TRULDEPds
123700060925     c                   move      'L'           IdepTLA
123800060925     c                   move      Odepot        IdepDPC
123900060925     c                   call      'TRULDEPR'
124000060925     C                   PARM                    TRULDEPds
124100090522      *
124200061129     C                   IF        OdepERR <>'1'  and OdepKSC >0
124300060925     c                   move      OdepKSC       cod07n            7 0
124400060925     c                   else
124500090522      *
124600060925     c                   z-add     §ptKSC        cod07n            7 0
124700090522      *
124800090522      ** avvisa REA e INT + Bertocchi x Legame non trovato
124900090522     c                   exsr      email_alert
125000060925     c                   end
125100060925      **
125200060808     c                   z-add     0             cod10n           10 0
125300060808     c                   movel     Cod07n        cod10n
125400060925      **
125500060808     c                   movel     'N'           trova_cod         1
125600060808      **
125700060808     C* ?Decodifica  Codice Ordinante su Anagrafica clienti di Ritiro
125800060814     C* ?------------------
125900060911      *?    DA SOSTITUIRE APPENA pronta utility per reperire da DPPDC00F
126000060808     C     cod10n        setll     Fnacr01l
126100060808     C                   read      Fnacr01l
126200060808      **
126300060808     c                   Dow       not %EoF(Fnacr01l)
126400061129     c                   movel     ACRCRO        par07n            7 0
126500060808      **
126600060808      **  Se diverso deve uscire immediatamente
126700060808     c                   If        par07n <> cod07n
126800060808     c                   leave
126900060808     c                   Else
127000060808      **  Se uguale invece deve essere non bloccato (M)
127100060808     c                   iF        ACRTCR <> 'M'
127200060808     c                   movel     'S'           trova_cod
127300060808     c                   leave
127400060808     c                   endiF
127500060808     c                   endIf
127600060808      **
127700060808     C                   read      Fnacr01l
127800060808     c                   endDO
127900060814     C* ?------------------
128000060808      **
128100060808     c                   if        trova_cod = 'S'
128200060808      ** se ha decodificato l'ordinante cerca sull'anagrafica dei ritiri
128300060808     c                   move      *zeros        vaoCOR
128400060808     c                   movel     ACRCRO        vaoCOR
128500060808     c                   else
128600060808     c                   z-add     1             acokut
128700060808     c                   z-add     151           acokcc
128800060808     c                   z-add     Cod07n        acoksc
128900060808     C     KAco          chain     Cnaco00F
129000060808     c                   if        %found(Cnaco00F)
129100060808      ** se ha decodificato l'ordinante cerca sull'anagrafica dei ritiri
129200060808     c                   move      *zeros        vaoCOR
129300060808     c                   movel     Cod07n        vaoCOR
129400060808     C                   Else
129500060808     C*
129600061002     C                   eval      vaorso = ONAME_maiu
129700061002     C                   eval      vaoino = OSTREET_maiu
129800060808     C                   eval      vaocao = OPOSTAL
129900061002     C                   eval      vaoloo = OCITY_maiu
130000061006     C* CAP OBBLIGATORIO : CONTROLLO
130100061006     C                   CLEAR                   DSSI95
130200061006     C                   CLEAR                   DSLV13
130300061006     C                   MOVEL     '3'           I95TCN
130400061006     C                   MOVEL     vaocao        I95CAP
130500061006     C                   MOVEL     vaoloo        I95LOC
130600061006     C* ERRORE AFFIDABILITA = 0
130700061006     C                   MOVEL     'S'           I13AF0
130800061006     C                   MOVEL     'S'           I13AF1
130900080430     C                   z-add     1             E13FZ1
131000061006     C                   CALL      'FNLV13R'
131100061006     C                   PARM                    KPJBA
131200061006     C                   PARM                    DSLV13
131300061006     C                   PARM                    DSSI95
131400061006     C* ERRORE
131500061006     C     O13ERR        IFEQ      *BLANKS
131600061006     C                   MOVEL     O95PRV        vaoPRo
131700061006    3C                   ENDIF
131800061006     C*
131900060808     C* Nazione Ordinante
132000060808     C                   z-add     1             Nz
132100061002     C     oCountry      lookup    Cie03(Nz)                              99
132200080404DO  1C                   if        %Equal
132300060808     C                   MOVEL     Naz03(Nz)     vaoNAO
132400060808E   1C                   End
132500060808     C*
132600060808     C                   End
132700060808     C                   End
132800060808      **
132900060808      **  ORM Commissionato
133000060808     c                   move      'S'           §orComC
133100060808     c                   move      'S'           §ormFD
133200080423     c                   move      'S'           §ormFR
133300080423     c*
133400060808     c                   eval      vaoFLO = Dorm01
133500061002     C* ?Appunto se DPD fa ORM commissionato:             --> il flag = "S"
133600061002     C* ?  DPD deve inviare per ORM commissionato delle informazioni precise:
133700061002     C* ?  - Peso del collo.
133800061002     C* ?  - Ora dalla quale il nostro Autista può andare a ritirare
133900061002     C* ?  - Il Referente da contattare
134000061002     C* ?  - Il Telefono del Referente
134100061002     C* ?Se invece l'Orm non Fosse commissionato, significa che già DPD ha preso i
134200061002     C* ? contatti e noi dobbiamo solo andare a ritirare. --> il flag = "N"
134300060808      **
134301160215      **   CHIODO:
134302160215      **     x correggere se in consegna in PORTOGALLO e con CAP inferiore a 7 caratteri
134303160215      **     imposta il codice completandolo con degli zeri dopo il codice (quindi alla dx)
134304160215     c                   if        vaoNAC = 'P  '
134305160215     c                               and
134307160215     c                             %len(%trim(vaoCAC)) < 7
134308160215      **  riempe con '0'  es.:XXXX000
134309160215     c                   z-add     0             da                1 0
134310160215     c                   z-add     0             in                1 0
134312160215      *
134313160215     c                   eval      da = %len(%trim(vaoCAC)) + 1
134314160215     c     da            do        7             in
134316160215     c                   eval      %subst(vaoCAC:in:1)='0'
134318160215     c                   Enddo
134319160215     c                   End
134320160215      **
134400060808     c                   write     fnvao000
134500060808     c                   move      'S'           Almeno_Uno
134600060808      **
134700060808     c                   Endsr
134800140929      * ?------------------------------------------------------------------ */
134900140929      *?   Cerca la LINEA TO o VR da
135000140929      * ?------------------------------------------------------------------ */
135100140929     c     NAZ_da_oDepot Begsr
135200140929      **
135300140930     c                   z-add     0             ISOnazCAP         3 0
135400140929      /free
135500140930          // Sul cappario nella versione in vigore alla data
135600140930          // il Depot appartiene alla Nazione xxx
135700140929
135800140930               EXEC SQL select CCCisok INTO :ISOnazCAP
135900140930                 FROM DPCCC00F WHERE
136000140930                 CCCiso2 concat digits(CCCver) in
136100140930                   (SELECT CROiso2 concat digits(CROver)
136200140930                    FROM DPCRO00F WHERE  CROddep = :oDepot
136300140930                    AND CROver in
136400140930                      (SELECT CVEver FROM DPCVE00F WHERE
136500140930                       CVEdde<= :data_oggi and CVEdsc >= :data_oggi)
136600140930                     GROUP BY CROISO2, crover)
136700140930
136800140929                       FETCH FIRST 1 ROW ONLY
136900140929                  ;
137000140929
137100140929                  IF sqlCode < 0;
137200140929                    DUMP(A);
137300140929                  ENDIF;
137400140929
137500140930                  IF sqlCode = 0 and ISOnazCAP > 0;
137600140930
137700140930                       eval Nz = 1;
137800140930                       Nz = %LOOKUP(ISOnazCAP:Cie03);
137900140930
138000140930            // se ha trovato imposta il campo di passaggio codice x il POE
138100140930                            If %Equal and POE03(Nz) > 0;
138200140930                                eval in_POE = POE03(Nz);
138300140930                            endif;
138400140930
138500140930                       leavesr;
138600140929                  ENDIF;
138700140929
138800140929      /end-free
138900140929      **
139000140929     c                   Endsr
139100060808      * ?------------------------------------------------------------------ */
139200060808      *?   Se richiesto di stornare un ORM
139300060808      * ?------------------------------------------------------------------ */
139400060808     c     STORNO        Begsr
139500060808      **
139600060808     c                   movel     ODEPOT        Key_DPDORM
139700060808     c                   move      ORDERNR       Key_DPDORM
139800060808      **     pgm di storno ORDINI DPD
139900060808     c                   call      'FIEU46R'
140000060809     c                   parm                    Key_DPDORM       10
140100060808      **
140200060808     c                   Endsr
140300060612      * ?------------------------------------------------------------------ */
140400060612      *?      Suddivide i campi della riga con carattere divisorio (;)
140500060612      * ?------------------------------------------------------------------ */
140600060612     c     Split_Rec     Begsr
140700060612      **
140800060612     c                   clear                   Nr_campi
140900060612     c                   clear                   Campi_Record
141000060612     c                   clear                   Campi_Lunghi
141100060612     c                   clear                   Campi_Numerici
141200060612     c                   clear                   Campi_Decimali
141300060612     c                   movel     ';'           Separa_campi
141400060612     c                   movel     '.'           Decimal_Separ
141500060612      *
141600060612     c                   call      'FIEU00R'
141700060612      * ?     Input
141800060612     c                   parm                    Separa_campi
141900060612     c                   parm                    Decimal_Separ
142000060612     c                   parm                    dati
142100060612      * ?     Output  su schiere di 100 elementi
142200060612     c                   parm                    Nr_campi
142300060612     c                   parm                    Campi_Record                   * i dati
142400060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
142500060612     c                   parm                    Campi_Numerici                 * se numerici
142600060612     c                   parm                    Campi_Decimali                 * quanti decimali
142700060612      **
142800060612     c                   Endsr
142900971215      *----------------------------------------------------------------
143000051205      *   DEFINIZIONE CHIAVI                               *
143100060808      *----------------------------------------------------------------
143200051205     C     *INZSR        BEGSR
143300051205      *
143400991129     c     *ENTRY        PLIST
143500060613     C                   parm                    esito             1
143600991129      *
143700060808     C     Key_Tbl       klist
143800060808     c                   kfld                    tblKUT
143900060808     c                   kfld                    tblCOD
144000060808     c                   kfld                    tblKEY
144100060808      *
144200060808     C     Key_Tbl1      klist
144300060808     c                   kfld                    tblKUT
144400060808     c                   kfld                    tblCOD
144500060808      *
144600060808     C     KAco          Klist
144700060808     c                   Kfld                    acokut
144800060808     c                   Kfld                    acokcc
144900060808     c                   Kfld                    acoksc
145000060925      *
145100060925     C     Kpt190        Klist
145200060925     c                   Kfld                    tabCOD
145300060925     c                   Kfld                    tabKEY
145400080430      *
145500080430     C     dp3_Ordine    Klist
145600080430     c                   Kfld                    Dp3DEP
145700080430     c                   Kfld                    Dp3ORD
145800060808      *
145900060808      *  imposta UDATE di elaborazione
146000060915     C                   move      *date         data_oggi
146100060915     C                   move      *date         G02inv
146200060915     C                   MOVE      *ZEROS        G02dat
146300060808     C                   MOVE      *BLANKS       G02ERR
146400060808     C                   CALL      'XSRDA8'
146500060808     C                   PARM                    WLBDAT
146600060808     C                   move      data_oggi     day
146700060808     C                   TIME                    W0140
146800060808     C                   MOVEl     W0140         ore
146900060808      *
147000060808      * Nazione di consegna x linea DPD x P.O. di Consegna ed Emittente
147100060808     C                   z-add     0             Nz                3 0
147200060808     c                   clear                   Naz03
147300060808     c                   clear                   Iso03
147400060808     c                   clear                   DpD03
147500060808     C                   z-add     1             tblKUT
147600060808     C                   MOVEL(p)  '15'          tblCOD
147700060808     C     Key_Tbl1      setll     TABEL00F
147800060808     C     Key_Tbl1      reade     TABEL00F
147900060808DO  1C                   Dow       not %EoF(TABEL00F)
148000060808DO  1C                   if         tblflg = *blank
148100060808     C                   MOVEL     TBLuni        DS15
148200060808     C                   add       1             Nz
148300060808     C                   MOVEL     tblKEY        Naz03(Nz)
148400060808     C                   MOVEL     §15COD        Iso03(Nz)
148500060808     C                   MOVEL     §15LAD        DpD03(Nz)
148600060809     C                   MOVEL     §15CIE        Cie03(Nz)
148700080604     C                   MOVE      *zeros        POE03(Nz)
148800080604     c                   if        §15dpdPOE <> *blank
148900080604     c     digits        check     §15dpdPOE
149000080528     c                   if        not %Found
149100080604     C                   MOVE      §15dpdPOE     POE03(Nz)
149200080604E   1C                   End
149300080604     c                   if        POE03(Nz) = 0
149400080604     C                   z-add     190           POE03(Nz)
149500080528E   1C                   End
149600080527E   1C                   End
149700060808E   1C                   End
149800060808     C     Key_Tbl1      reade     TABEL00F
149900060808E   1C                   End
150000050414      *
150100080505      * decodifica tabella PT
150200080505     c                   clear                   edidsPT
150300080505     c                   move      'PT'          tabCOD
150400080505     c                   movel(p)  'DPD190'      tabKEY
150500080505     c     Kpt190        chain     EDtab01l
150600080505     c                   if        %Found(Edtab01l)
150700080505     c                   movel     tabUNI        edidsPT
150800080505     c                   end
150900080505      *
151000991124     C                   ENDSR
151100060621      * ?------------------------------------------------------------------ */
151200090522      *?  Invio Avviso x mancanza Legame fra Depot e Cliente Bartolini
151300060621      * ?------------------------------------------------------------------ */
151400090522     C     eMail_alert   BEGSR
151500090601     C*
151600090601     C                   eval      wrkOgg='Depot ' + Odepot +' NON presente sul-
151700090601     C                             la gestione Depot DPD/Codice Cliente.'
151800090601     C*
151900090601     C                   eval      wrkMsg='E'' stato ricevuto un ORM di DPD da'+
152000090601     C                             'l Depot ' + ODEPOT + '.   Attenzione, il ' +
152100090601     C                             'Depot non è presente sulla nostra tabella '+
152200090601     C                             'dei Lagami Depot/Codice Cliente. '         +
152300090601     C                             'Utilizzando la funzione: ( '               +
152400090601     C                             'Gestione legami Depot DPD/Codice Cliente )'+
152500090601     C                             ', inserire il legame per il Depot '        +
152600090601     C                             'Mancante.  Al momento l''ORM chiesto dal ' +
152700090601     C                             'DEPOT ' + ODEPOT + ' prenderà il cliente ' +
152800090601     C                             'Ordinante generico.'
152900090601     C*
153000090601     c                   if        VaoPOE = 190
153100121129     C                   eval      wrkEml   = 'INT043@brt.it'
153200121129     C                   eval      wrkEmlcc = 'REA043@brt.it'
153300150325     c                   elseif    VaoPOE = 194
153400150325     C                   eval      wrkEml   = 'INT002@brt.it'
153500150325     C                   eval      wrkEmlcc = 'REA002@brt.it'
153600090601     c                   else
153700150325      * i rimanenti devono essere 195
153800121129     C                   eval      wrkEml   = 'INT049@brt.it'
153900121129     C                   eval      wrkEmlcc = 'REA049@brt.it'
154000090601     c                   end
154100090601      *
154200121129     C                   call(e)   'TIS701C1'
154300090601     C                   parm                    wrkEml          253
154400121129     C                   parm                    wrkEmlCc        253
154500090601     C                   parm                    wrkOgg           44
154600090601     C                   parm                    wrkMsg         5000
154700121129     C                   parm                    wrkesito          1
154800090522      *
154900090522     C                   ENDSR
155000090522      * ?------------------------------------------------------------------ */
155100090522      *?      X non bloccare in nessun caso il traduttore CLIENTI
155200090522      * ?------------------------------------------------------------------ */
155300090522     C     *pssr         BEGSR
155400090522     C
155500060710     C                   eval      esito = '2'
155600060621     C
155700060621     C                   ENDSR     '*CANCL'
155800060614      *-------------------------------------------------------------------------
