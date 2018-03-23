000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060609      **?************************************************************************
000500060613      *  Da UPLOAD                                                              *
000600060808      *  Importa da FTP server i Collection request DPD                         *
000700060809      *  TRASCODIFICA : CRDATA02      IMPORT DPD nuovo parcel e depot           *
000800060612      **?************************************************************************
000900060612     Ftivin00r  uF   E             DISK    usropn
001000060809      *
001100060809     FFIDPO03L  uF   E           k DISK
001200060809     FFnORM01L  uF   E           k DISK
001300060809     FFnORF01L  iF a E           k DISK
001400061017     Ffiar401l  if   E           k disk    rename(FIAR4000:FIAR4001)
001500061017     Ffiar403l  if   E           k disk
001600060906     FTntbe01L  iF   E           k DISK
001700060808      * ?================================================================== */
001800060612     D Lung_Parcel     S              3  0
001900060620     D Parcel11        S             11
002000060710     D Parcel14        S             14
002100060809     D Tipo_error      S              1  0
002200080516     D Consegna_estero...
002300080516     D                 S              2    INZ('82')
002400060808      * ?================================================================== */
002500060808     D  data_Oggi      s              8  0 INZ(0)
002600060808     D  day            s              8  0 INZ(0)
002700060808     D  ore            s              6  0 INZ(0)
002800060808     D  w0140          s             14  0 INZ(0)
002900060809      *
003000060808     D WLBDAT          DS
003100060808     D  G02DAT                 1      8  0
003200060808     D  G02INV                 9     16  0
003300060808     D  G02ERR                17     17
003400060808     D  G02TGI                18     22  0
003500060906     D x               s              3s 0
003600060906     D mancod          s              2
003700060906     D manfase         s              3s 0
003800060906     D manb            s              2    dim(50)
003900060906     D mand            s              3s 0 dim(50)
004000060906     D manf            s              3s 0 dim(50)
004100060906     D dmrd          E DS
004200061017     D dsbl4I        E DS
004300070612     D dsbl4M        E DS
004400060809      * ?   *--------------------------------------------------------------*
004500060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
004600060809      * ?   *--------------------------------------------------------------*
004700060809     D FIDPODTA2     e DS
004800061002     D     Zcode5             61     63  0
004900061002     D     Zcode4             64     66  0
005000060809     D     Zcode3             67     69  0
005100061002     D     Zcode2             70     72  0
005200061002     D     Zcode1             73     75  0
005300080404     D     Zcod               61     75  0 DIM(5)
005400080404      *
005500080404      * ?( add.code ==> [82] Parcel in più )
005600080404     D Parcel_inPIU    s              3s 0 inz(82)
005700080404      *
005800080423      * ?   *--------------------------------------------------------------*
005900080423      *         per riapertura automatica dell'ORM  --> FIOR51R
006000080423      * ?   *--------------------------------------------------------------*
006100080423     D FIOR51DS      e DS
006200060612      * ?================================================================== */
006201170102     d FIEU39DS      e DS
006202170102
006300060809      * ?   * Campi da tradurre con interi
006400060809     D atoll           PR            20I 0
006500060809     D                                     EXTPROC('atoll')
006600060809     D  Char                           *
006700060809     D                                     VALUE
006800060809     D                                     OPTIONS(*STRING)
006900060612      * ?   * Campi da tradurre con interi
007000060612     D atoi            PR            10I 0
007100060612     D                                     EXTPROC('atoi')
007200060612     D  Char                           *
007300060612     D                                     VALUE
007400060612     D                                     OPTIONS(*STRING)
007500060612      * ?   * Campi da tradurre con decimali
007600060612     D atof            PR             8F
007700060612     D                                     EXTPROC('atof')
007800060612     D  Char                           *
007900060612     D                                     VALUE
008000060612     D                                     OPTIONS(*STRING)
008100060612      * ?   *--------------------------------------------------------------*
008200060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
008300060612      * ?   *--------------------------------------------------------------*
008400060612      *
008500060612      * ?   * Campi x decodifica * (INPUT  del Record)
008600060612     D  Dati           s           1500
008700060612     D Separa_campi    s              1
008800060612     D Decimal_Separ   s              1
008900060809      *
009000060612      * ?   * Campi decodificati * (OUTPUT del Record)
009100060612      *       Schiere di Output / Campi di Output
009200060614     D  Alfa40         s             40
009300060612     d  Sk             s              3u 0
009400060612     D    NR_campi     s              3u 0
009500060612     D  Campi_Record   ds
009600060612     D    Dato_sk                    40    DIM(100)
009700060612     D  Campi_Lunghi   ds
009800060612     D    LunDato_sk                  3u 0 DIM(100)
009900060612     D Campi_Numerici  ds
010000060612     D    DatoNum_sk                  1    DIM(100)
010100060612     D Campi_Decimali  ds
010200060612     D    Decimal_sk                  3u 0 DIM(100)
010300060612      * ?   *--------------------------------------------------------------*
010400060612     D  position       s              3  0 INZ(0)
010500060612
010600060612     D  se_errore      s              1    inz(' ')
010700060612     D  Msg_Err        s            132    inz(' ')
010800060614     D  Msg_Vin_80     s             80    inz(' ')
010900060612      * ?* ------------------------------------------------------ *
011000060612      * Records di controllo mandati nell'IGATEAVIS
011100060612      * ?   * Dati Record UNO *
011200060612     DDepot_Trs_UNO    s             10
011300060612     D User_Trs_UNO    s             10
011400060612     D Data_Trs_UNO    s              8  0
011500060612     D Time_Trs_UNO    s              6  0
011600060612     D  Num_Trs_UNO    s              9  0
011700060809      * ?   * Dati Record DUE *
011800060809     D Vers_Trs_DUE    s             15
011900060809      * ?   * Dati Record TRE *
012000060612     D Testa_Sk        s             10    DIM(50)
012100060809      * ?   * Dati Record QUATTRO *
012200060612     D  Num_Trs_FIN    s              9  0
012300060710      **
012400060710     D Digits          C                   '0123456789'
012500060612      * ?================================================================== */
012600060612      *   Ciclo principale
012700060413      * ?********************************************************************/
012800060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
012900060612     C*
013000060612     C                   if        not %open(tivin00r)
013100060612     C                   open      tivin00r
013200060612     C                   endif
013300060612      **
013400060612      * ?------------------------------------------------------------------ */
013500060612     C*? Controllo dati arrivati da DPD
013600060612      * ?------------------------------------------------------------------ */
013700060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
013800060612      * ?- controllando che la trasmissione sia completa.
013900060612
014000060612      * ?              /*---------------------- */
014100060612     c                   exsr      check_Trasm
014200060612      * ?              /*---------------------- */
014300060612      **
014400060613      **  Errore di trasmissione x tutti i records
014500060613      **   --> file in errore
014600060613     c                   if        se_errore = 'S'
014700060612
014800060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
014900060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
015000060613     C                               >> Farsi rimandare i DATI !!'
015100060612
015200060612      * ?  Scrive su tutti i records il tipo di errore
015300060612     c     *start        setll     tivin00r
015400060612     c                   read      tivin00r
015500060612     c                   dow       not %eof(tivin00r)
015600060612     C                   eval      vinMSG = Msg_Err
015700060612     C                   eval      vinFLG = '2'
015800060613     c                   eval      esito  = '2'
015900060612     c                   update    tivin000
016000060612     c                   read      tivin00r
016100060612     c                   endDO
016200060612      **
016300060612     c                   else
016400060614      **
016500060612      * ?------------------------------------------------------------------ */
016600060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
016700060612      * ?------------------------------------------------------------------ */
016800060612      * ?              /*---------------------- */
016900060612     c                   exsr      Importa_Msg
017000060612      * ?              /*---------------------- */
017100060612
017200060614     c                   end
017300060614
017400060612     C                   if        %open(tivin00r)
017500060612     C                   close     tivin00r
017600060612     C                   endif
017700060612      *
017800060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
017900060614      *
018000060710     c                   if        almeno_uno ='S' and esito ='1'
018100060710     C                   eval      esito ='0'
018200060614     C                   endif
018201170102
018202170102     c                   eval      i39tla='C'
018203170102     c                   call      'FIEU39R'
018204170102     c                   parm                    fieu39ds
018300060614      *
018400060612     c                   SETON                                        LR
018500060612      * ?================================================================== */
018600060612      *? Controlla la trasmissione   se completa
018700060612      * ?------------------------------------------------------------------ */
018800060612     c     Check_Trasm   Begsr
018900060612
019000060612     C                   clear                   se_errore
019100060612      ** primo  record
019200060612     c     *start        setll     tivin00r
019300060612     c                   read      tivin00r
019400060612     c                   if        not %eof(tivin00r)
019500060612      * ?              /*---------------------- */
019600060612     c                   exsr      Decod_Record
019700060612      * ?              /*---------------------- */
019800060612     c                   endif
019900060612      **
020000060612      ** ultimo record
020100060612     c     *hival        setll     tivin00r
020200060612     c                   readp     tivin00r
020300060612     c                   if        not %eof(tivin00r)
020400060612      * ?              /*---------------------- */
020500060612     c                   exsr      Decod_Record
020600060612      * ?              /*---------------------- */
020700060612     c                   endif
020800060612
020900060612      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
021000060612      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
021100060612      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
021200060612     C                   if        num_trs_UNO <> num_trs_FIN
021300060612      * ?-----> Errore
021400060612     C                   eval      se_errore = 'S'
021500060612
021600060612     c                   end
021700060612
021800060612     c                   endSR
021900060612      * ?------------------------------------------------------------------ */
022000060612      *? Importa i records della tramsissione
022100060612      * ?------------------------------------------------------------------ */
022200060612     c     Importa_Msg   Begsr
022300060612
022400060614      * per controllare se almeno un record è stato importato sul VAB
022500060614     c                   clear                   Almeno_Uno        1
022600060614
022700060612     c     *start        setll     Tivin00r
022800060612     c                   read      Tivin00r
022900060612
023000060612     c                   dow       not %eof(Tivin00r)
023100060614
023200060614      * solo i record sflaggati da rielaborare
023300060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
023400060612
023500060612      ** Controlli formali sui campi
023600060612     c                   clear                   se_errore
023700060620     C                   clear                   Msg_Vin_80
023800060612
023900060612      ** Decodifica record a campi variabili
024000060612      * ?              /*---------------------- */
024100060612     c                   exsr      Decod_Record
024200060612      * ?              /*---------------------- */
024300060612
024400060612      **  Se presente un errore nel record emette una segnalazione msg
024500060612     c                   if        se_errore ='S'
024600060614     C                   eval      vinMSG = Msg_Vin_80
024700060621     c                   end
024800060612
024900060809      *  con il record in canna scrive direttamente il VAO
025000060620     c                   if        %subst(dati:1:1) <> '#'
025100060612      * ?              /*---------------------- */
025200060809     c                   exsr      WRT_FILE
025300060612      * ?              /*---------------------- */
025400060620     c                   end
025500060614
025600060809      *  Problemi nella decodifica dei campi VAO
025700060614     c                   if        se_errore ='S'
025800060614     C                   eval      vinMSG = Msg_Vin_80
025900060614     c                   end
026000060612
026100060621      *  Sempre Record OK
026200060621     C                   eval      vinFLG = '1'
026300060710     c                   eval      esito  = '0'
026400060621
026500060809      *  x errore bloccante nella composizione del VAO
026600060621     c                   if        err_bloccante ='S'
026700060621     C                   eval      vinFLG = '2'
026800060710     c                   eval      esito  = '1'
026900060621     c                   end
027000060612
027100060612     c                   update    Tivin000
027200060614     c                   endIF
027300060612
027400060612     c                   read      Tivin00r
027500060612     C                   ENDdo
027600060612      **
027700060612     c                   endSR
027800060612      * ?------------------------------------------------------------------ */
027900060612      *?    Decodifica record a campi variabili
028000060612      * ?------------------------------------------------------------------ */
028100060612     c     Decod_Record  Begsr
028200060612
028300060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
028400060612     c                   movel(p)  VINDTA        dati
028500060612      **
028600060612      * ?              /*---------------------- */
028700060612     c                   exsr      Split_Rec
028800060612      * ?              /*---------------------- */
028900060612      **
029000060612      **  Records di Intestazione e di fine Msg
029100060612     c                   if        %subst(dati:1:1) ='#'
029200060612      * ?              /*---------------------- */
029300060612     c                   exsr      Rec_Speciali
029400060612      * ?              /*---------------------- */
029500060612     c                   Else
029600060612      ** conta le righe
029700060612      * ?              /*---------------------- */
029800060612     c                   exsr      Rec_Dettaglio
029900060612      * ?              /*---------------------- */
030000060612     c                   End
030100060612      **
030200060612     c                   Endsr
030300060612      * ?------------------------------------------------------------------ */
030400060612      *?      Record di Testata e di Fine Msg
030500060612      * ?------------------------------------------------------------------ */
030600060612     c     Rec_Speciali  Begsr
030700060612      **
030800060612      * ?  Prima riga
030900060612     c     'FILE'        SCAN      dati:2        position
031000060612     c                   if        %Found and position < 5
031100060612     c                   exsr      Rec_UNO
031200060612     c                   ElSe
031300060612     c                   End
031400060612      **
031500060612      * ?  Seconda riga (testate dei campi)
031600060809     c     'Version'     SCAN      dati:2        position
031700060809     c                   if        %Found and position < 5
031800060809     c                   exsr      Rec_DUE
031900060809     c                   End
032000060809      **
032100060809      * ?  Terza riga (testate dei campi)
032200060612     c     'DEF'         SCAN      dati:2        position
032300060612     c                   if        %Found and position < 5
032400060809     c                   exsr      Rec_TRE
032500060612     c                   End
032600060612      **
032700060612      * ?  Ultima riga
032800060612     c     'END'         SCAN      dati:2        position
032900060612     c                   if        %Found and position < 5
033000060612     c                   exsr      Rec_LAST
033100060612     c                   End
033200060612      **
033300060612     c                   Endsr
033400060612      * ?------------------------------------------------------------------ */
033500060612      *?      Primo Record
033600060612      * ?------------------------------------------------------------------ */
033700060612     c     Rec_UNO       Begsr
033800060612      **
033900060612      ** esempio di record :
034000060808      * ?   # FILE;N00;depot945;20050603;143526;6;
034100060612      **
034200060612      **  Inizializza i campi della trasmissione
034300060612     C                   eval      Depot_Trs_UNO = *blank
034400060612     C                   eval      User_Trs_UNO  = *blank
034500060612     C                   eval      Data_Trs_UNO  = 0
034600060612     C                   eval      Time_Trs_UNO  = 0
034700060612     C                   eval      Num_Trs_UNO   = 0
034800060612
034900060612      **   Memorizza il numero della trasm. da confrontare con l'ultimo record.
035000060612      **     Tutto il dettaglio deve essere compreso entro questi 2 records
035100060612      **    altrimenti il msg.non è completo e bisogna farsi rimandare il file.
035200060612
035300060808      * ? Nel 2° campo di testata c'è lo USER in Delis
035400060808     C                   z-add     2             sk
035500060808     c                   eval      User_Trs_UNO = %subst(Dato_sk(sk):1:+
035600060808     c                             LunDato_sk(sk))
035700060808
035800060808      * ? Nel 3° campo di testata c'è il Dispatch Depot
035900060808     C                   z-add     3             sk
036000060808     c                   eval      Depot_Trs_UNO = %subst(Dato_sk(sk):1:+
036100060808     c                             LunDato_sk(sk))
036200060808
036300060808      * ? Nel 4° campo di testata c'è la data trasmissione
036400060808     C                   z-add     4             sk
036500060808     c                   if        DatoNum_sk(sk) = 'S'
036600060808     c                   eval      Data_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
036700060808     c                   End
036800060808
036900060808      * ? Nel 5° campo di testata c'è il nr.seriale di trasmissione
037000060808     C                   z-add     5             sk
037100060808     c                   if        DatoNum_sk(sk) = 'S'
037200060808     c                   eval      Time_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
037300060808     c                   End
037400060808
037500060808      * ? Nel 6° campo di testata c'è il nr.seriale di trasmissione
037600060808     C                   z-add     6             sk
037700060808     c                   if        DatoNum_sk(sk) = 'S'
037800060808     c                   eval      num_trs_UNO = atoi(%TRIM(Dato_sk(sk)))
037900060808     c                   End
038000060612      **
038100060612     c                   Endsr
038200060809      * ?------------------------------------------------------------------ */
038300060809      *?      Secondo Record
038400060809      * ?------------------------------------------------------------------ */
038500060809     c     Rec_DUE       Begsr
038600060809      **
038700060809      ** esempio di record :
038800060809      * ?   # Version;JR20040831;
038900060809      **
039000060809      **  Inizializza i campi della versione trasmissione
039100060809     C                   eval      Vers_Trs_DUE = *blank
039200060809      **
039300060809      **   Memorizza la versione i trasm.
039400060809
039500060809      * ? Nel 2° campo di testata c'è lo USER in Delis
039600060809     C                   z-add     2             sk
039700060809     c                   eval      Vers_Trs_DUE = %subst(Dato_sk(sk):1:+
039800060809     c                             LunDato_sk(sk))
039900060809      **
040000060809     c                   Endsr
040100060612      * ?------------------------------------------------------------------ */
040200060809      *?      Terzo   Record
040300060612      * ?------------------------------------------------------------------ */
040400060809     c     Rec_TRE       Begsr
040500060612      **
040600060612      ** esempio di record :
040700060808      * ?   # DEF;CR:CRDATA01;ODEPOT;ORDERNR;CDEPOT;OCUSTNO;ONAME;ONAME2;OS...
040800060612      **
040900060612      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
041000060612     C                   clear                   Testa_Sk
041100060612
041200060612      * ? Dal 3° campo troviamo i nomi delle intestazioni dei campi C.Request
041300060612     C     3             do        50            sk
041400060612     c                   eval      Testa_Sk(sk-2) = %subst(Dato_sk(sk):1:+
041500060612     c                             LunDato_sk(sk))
041600060612     c                   enddo
041700060612      **
041800060612     c                   Endsr
041900060612      * ?------------------------------------------------------------------ */
042000060612      *?      Ultimo  Record
042100060612      * ?------------------------------------------------------------------ */
042200060612     c     Rec_LAST      Begsr
042300060612      **
042400060612      ** esempio di record :
042500060808      * ?   # END;6;
042600060612      **
042700060612      **  Inizializza i campi della trasmissione
042800060612     C                   eval      Num_Trs_fin = 0
042900060612      **
043000060612      **  Nell'ultimo record di chiusura deve esserci lo stesso numero di trasmissione
043100060612      **  impostato nel primo record.
043200060612      **
043300060612      * ? Nel 2° campo di fine trsm. c'è il nr.seriale di trasmissione
043400060612     C                   z-add     2             sk
043500060612     c                   if        DatoNum_sk(sk) = 'S'
043600060612     c                   eval      num_trs_fin = atoi(%TRIM(Dato_sk(sk)))
043700060612     c                   End
043800060612      **
043900060612     c                   Endsr
044000060612      * ?------------------------------------------------------------------ */
044100060612      *?      Riga Bolla Import da controllare e tradurre
044200060612      * ?------------------------------------------------------------------ */
044300060612     c     Rec_Dettaglio Begsr
044400060612      **
044500060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
044600060612      **
044700060809     c                   clear                   fidpoDTA2
044800060614     c                   clear                   Msg_Err
044900060809      **
045000060809      * ? Nel 1° campo : Numero Requesting Depot     (Oblligatorio)  ODEPOT
045100060809      **                                                             DEPOT - ORDINANTE
045200060809     C                   z-add     1             sk
045300060809     c                   If        LunDato_sk(sk) =  0
045400060809      * ?-----> Errore
045500060809     C                   eval      tipo_error = 1
045600060809     C                   exsr      Field_Error
045700060809     c                   Else
045800060809     c                   eval      ODEPOT = %subst(Dato_sk(sk):1:+
045900060809     c                                              LunDato_sk(sk))
046000060809     c                   End
046100060809      **
046200060809      * ? Nel 2° campo : Numero Ordine DPD           (Oblligatorio)  ORDERNR
046300060809      **   parte 2 della chiave ORM DPD                              NUM.ORDINE
046400060809     C                   add       1             sk
046500060809     c                   If        DatoNum_sk(sk) = 'N' or
046600060809     c                             LunDato_sk(sk) =  0
046700060809      * ?-----> Errore
046800060809     C                   eval      tipo_error = 2
046900060809     C                   exsr      Field_Error
047000060809     c                   Else
047100060809     c                   eval      ORDERNR= atoi(%TRIM(Dato_sk(sk)))
047200060809     c                   End
047300060809      **
047400060809      * ? Nel 3° campo : Depot Che deve eseguire ORM                 CDEPOT
047500060809      **                                                             DEPOT DI RITIRO
047600060809     C                   add       1             sk
047700060809     c                   If        LunDato_sk(sk) =  0
047800060809      * ?-----> Errore
047900060809     C                   eval      tipo_error = 1
048000060809     C                   exsr      Field_Error
048100060809     c                   Else
048200060809     c                   eval      RDEPOT = %subst(Dato_sk(sk):1:+
048300060809     c                                              LunDato_sk(sk))
048400060809     c                   End
048500060809      **
048600060809      * ? Nel 4° campo : Parcel Number                               PKNR
048700060809      **                                                             PARCEL NR
048800060809     C                   add       1             sk
048900060809     c                   If        LunDato_sk(sk) =  0
049000060809      * ?-----> Errore
049100060809     C                   eval      tipo_error = 1
049200060809     C                   exsr      Field_Error
049300060809     c                   Else
049400060809     c                   eval      PKNR   = %subst(Dato_sk(sk):1:+
049500060809     c                                              LunDato_sk(sk))
049600060809      * ?   ( Il parcel non è + numerico )   controlla lunghezza del Parcel
049700060809     c                   clear                   Lung_Parcel
049800060809     c                   eval      Lung_Parcel = %Len(%trim(PKNR))
049900060809     c                   if        Lung_Parcel = 11
050000060809     C                   MOVEL     PKNR          parcel11
050100060809     c                   end
050200060809     c                   if        Lung_Parcel = 14
050300060809     C                   MOVEL     PKNR          parcel14
050400060809     c                   end
050500060809      **
050600060809     c                   End
050700090227      **
050800090227      * ? Nel 5° campo : Data Creazione Ordine                       NEUDAT
050900090227      **                                                             DATA CREAZIONE ORDINE
051000090227     C                   add       1             sk
051100090227     c                   If        DatoNum_sk(sk) = 'N' and
051200090227     c                             LunDato_sk(sk) >  0
051300090227      * ?-----> Errore
051400090227     C                   eval      tipo_error = 3
051500090227     C                   exsr      Field_Error
051600090227     c                   Else
051700090227     c                   eval      NEUDAT  =atoi(%TRIM(Dato_sk(sk)))
051800090227     c                   End
051900060809      **
052000090227      * ? Nel 6° campo : Data Ritiro                                 DATPCK
052100060809      **                                                             DATA RITIRO
052200060809     C                   add       1             sk
052300060809     c                   If        DatoNum_sk(sk) = 'N' and
052400060809     c                             LunDato_sk(sk) >  0
052500060809      * ?-----> Errore
052600060809     C                   eval      tipo_error = 3
052700060809     C                   exsr      Field_Error
052800060809     c                   Else
052900060809     c                   eval      DATPCK  =atoi(%TRIM(Dato_sk(sk)))
053000060809     c                   End
053100060809      **
053200060809      * ? Nel 7° campo : STATUS                      (Oblligatorio)  STATUS
053300060809      **                                                             status 1-2-3-4-5
053400060809     C                   add       1             sk
053500060809     c                   If        LunDato_sk(sk) =  0
053600060809      * ?-----> Errore
053700060809     C                   eval      tipo_error = 1
053800060809     C                   exsr      Field_Error
053900060809     c                   Else
054000060809     c                   eval      STATUS = %subst(Dato_sk(sk):1:+
054100060809     c                                              LunDato_sk(sk))
054200060809     c                   End
054300060809      **
054400060809      * ? Nel 8° campo : Peso (Con decimali)                         WEIGHT
054500060809      **                                                             PESO IN KG
054600060809     C                   add       1             sk
054700060809     c                   If        DatoNum_sk(sk) = 'N' and
054800060809     c                             LunDato_sk(sk) >  0
054900060809      * ?-----> Errore
055000060809     C                   eval      tipo_error = 3
055100060809     C                   exsr      Field_Error
055200060809     c                   Else
055300060809     c                   eval      WEIGHT = atof(%TRIM(Dato_sk(sk)))
055400060809     c                   End
055500060809      **
055600060809      * ? Nel 9° campo : SERVICE CODE                                SERVICE
055700060809      **                                                             SERVICE CODE
055800060809     C                   add       1             sk
055900060809     c                   If        DatoNum_sk(sk) = 'N' and
056000060809     c                             LunDato_sk(sk) >  0
056100060809      * ?-----> Errore
056200060809     C                   eval      tipo_error = 3
056300060809     C                   exsr      Field_Error
056400060809     c                   Else
056500060809     c                   eval      SERVICE= atoi(%TRIM(Dato_sk(sk)))
056600060809     c                   End
056700060809      **
056800060809      * ? Nel 10° campo : CUSTOM  status code                        CUSTOM
056900060809      **                                                             CUSTOM S.CODE
057000060809     C                   add       1             sk
057100060809     c                   If        LunDato_sk(sk) >  0
057200060809      * ?-----> Errore
057300060809     c                   eval      CUSTOM = %subst(Dato_sk(sk):1:+
057400060809     c                                              LunDato_sk(sk))
057500060809     c                   End
057600060809      **
057700060809      * ? Nel 11° campo : Codice Scan                                SCANT
057800060809      **                                                             SCAN
057900060809     C                   add       1             sk
058000060809     c                   If        DatoNum_sk(sk) = 'N' and
058100060809     c                             LunDato_sk(sk) >  0
058200060809      * ?-----> Errore
058300060809     C                   eval      tipo_error = 3
058400060809     C                   exsr      Field_Error
058500060809     c                   Else
058600060809     c                   eval      SCANT  = atoi(%TRIM(Dato_sk(sk)))
058700060809     c                   End
058800060809      **
058900060809      * ? Nel 12° campo : 5 Codici Addizionali                       ZCODE
059000060809      **                                                             COD.AGGIUNTIVI
059100060809     C                   add       1             sk
059200060809     c                   If        DatoNum_sk(sk) = 'N' and
059300060809     c                             LunDato_sk(sk) >  0
059400060809      * ?-----> Errore
059500060809     C                   eval      tipo_error = 3
059600060809     C                   exsr      Field_Error
059700060809     c                   Else
059800060809     c                   eval      ZCODE  = atoi(%TRIM(Dato_sk(sk)))
059900060809     c                   End
060000060808      **
060100060809      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL FIDPODTA2
060200060612      **
060300060612     c                   Endsr
060400060614      * ?------------------------------------------------------------------ */
060500060614      *?      Campi in errore
060600060614      * ?------------------------------------------------------------------ */
060700060614     c     Field_Error   Begsr
060800060614      **
060900060614     C                   eval      se_errore   = 'S'
061000060614      * msg video o in posta
061100060614     c                   If        Msg_Err = *blank
061200060614     c
061300060614     c                   Select
061400060614     c
061500060614     c                   When      Tipo_Error = 1
061600060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
061700060614     C                             ' >> Obblig.- Manca Dato !!'
061800060614     c
061900060614     c                   When      Tipo_Error = 2
062000060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
062100060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
062200060614     c
062300060614     c                   When      Tipo_Error = 3
062400060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
062500060614     C                             ' >> NON Numerico  !!'
062600060614     c
062700060614     c                   When      Tipo_Error = 4
062800060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
062900060614     C                             ' >> Conversione non riuscita !!'
063000060614     c                   EndSL
063100060614     c
063200060614     c                   End
063300060614      * Errore su VINMSG
063400060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
063500060614     c                                          + %trim(Testa_sk(sk))
063600060614      **
063700060614     c                   Endsr
063800060808      * ?------------------------------------------------------------------ */
063900060808      *?    Scrive i Files degli ORM
064000060808      * ?------------------------------------------------------------------ */
064100060809     c     WRT_FILE      Begsr
064200060809      **
064300060809      **  Se è in questo punto significa che non sono stati trovati errori
064400060809      **    formali quindi si passa gli status per avanzamento ORM
064500060809      **?              /*---------------------- */
064600060809     c                   ExSr      Avanza_ORM
064700060809      * ?              /*---------------------- */
064800060808      **
064900060809     c                   endSR
065000060809      * ?------------------------------------------------------------------ */
065100060809      *?    Imposta i campi per il VAO
065200060809      * ?------------------------------------------------------------------ */
065300060809     c     Avanza_ORM    Begsr
065400060809      **
065500060809     c                   clear                   err_bloccante     1
065600080605     c                   clear                   new_ormFAO        3 0
065700060906      * ?---------------------------------  **
065800060906      *?      ATTENZIONE: DA COMPLETARE.
065900060906      *?      COME GESTIRE I NUOVI STATUS
066000060906      *?       e GLI SCAN + COD.ADD.
066100060906      * ?---------------------------------  **
066200060906      **  SCAN
066300060906      **   SCAN + cod.addizionale
066400060906     c                   if        SCANT > 0
066500080404      **  ZCODE
066600061109     c                   clear                   mancod
066700061109     c                   clear                   manfase
066800080404     c                   clear                   ZcodeX            3 0
066900080404      *
067000080404     c                   if        ZCODE > 0
067100080404      *
067200080404      * ?/* subito controlla la presenza di un Parcel eccedente
067300080404      * ?   indicato con -->(82) */
067400080404     c                   z-add     1             x
067500080404     c     Parcel_inPIU  lookup    zcod(x)                                66
067600080404     c                   if        %Equal
067700080404     c                   z-add     Parcel_inPIU  zcode1
067800080404     c                   end
067900080404      *
068000080404      ** prendo in considerazione solo il primo valido
068100060906     c                   z-add     1             x
068200080404     c                   if        Zcode1 > 0
068300060906     c     zcode1        lookup    mand(x)                                66
068400080404     c   66              z-add     zcode1        ZcodeX
068500080404     c                   elseIf    Zcode2 > 0
068600080404     c     zcode2        lookup    mand(x)                                66
068700080404     c   66              z-add     zcode2        ZcodeX
068800080404     c                   elseIf    Zcode3 > 0
068900080404     c     zcode3        lookup    mand(x)                                66
069000080404     c   66              z-add     zcode3        ZcodeX
069100080404     c                   elseIf    Zcode4 > 0
069200080404     c     zcode4        lookup    mand(x)                                66
069300080404     c   66              z-add     zcode4        ZcodeX
069400080404     c                   elseIf    Zcode5 > 0
069500080404     c     zcode5        lookup    mand(x)                                66
069600080404     c   66              z-add     zcode5        ZcodeX
069700080404     c                   Endif
069800080404      *
069900080404     c                   if        %Equal
070000060906     c                   movel     manb(x)       mancod
070100060906     c                   movel     manf(x)       manfase
070200061109     c                   End
070300060906      **
070400060906     c                   End
070500060906      **
070600060906     c                   End
070700060920      * ?---------------------------------  **
070800060920      **  x DPD blank sullo status significa (1) (detto da DPD)
070900060920     c                   if        STATUS = *blank
071000060920     c                   move      '1'           STATUS
071100060920     c                   End
071200060920      **
071300060921      * ?- Aggancia il record del legame purchè non sia stato aggiornato
071400060921      * ?-  dall'altro programma con un Mancato Ritiro - SCAN 18.
071500060920     c     KDPO3         chain     fidpo03l
071600060920     c                   if        %Found(fidpo03l) and dpoSTS <> 8
071700080530     c                   clear                   DPOCODER
071800060920      **
071900060921      * ?- Controlla sull'ORM la Fase di avanzamento
072000060920     c     KOrM1         chain     fnOrm01l
072100060920      ** se addirittura non esiste il file  (Veramente Strano)
072200060920      **   imposta l'errore --> '10' x ricordare di non aver trovato l'ORM Bartolini
072300060920     c                   if        not %Found(fnorm01l)
072400060920     c                   movel     '10'          DPOCODER
072500060920     c                   else
072600060920      **
072700060921      * ?Controllo Fase ORM e/o aggiorna ORM Bartolini a seconda dello stato ricevuto
072800060920     c                   select
072900060920      **
073000060920      *?      Abbinato Parcel a Ordine DPD
073100060920     c                   when      STATUS = '1'
073200060920
073300060920     c                   if        ORmFAO <= 300
073400060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
073500060920     c                   z-add     400           ormFAO
073600060921     c                   exsr      aggior_fase
073700060921      *
073800060920     c                   else
073900061017      *
074000061017      * ?- Attenzione: occorre controllare se c'è stato un problema di ritardo dell'informazione
074100061017      * ?  di stato ricevuta in ritardo rispetto al pacco che può essere già arrivato come bolla
074200061017      * ?  import e quindi l'ORM può trovarsi già chiuso.
074300061017      * ? MA TUTTO è già stato trattato operativamente in modo corretto (NON DEVE SEGNALARE NULLA)
074400061017      * ?altrimenti
074500061017      * ? SE COSI'non fosse, occorre segnalare l'errore x essere preso in considerazione.
074600061017     c                   if        ORmFAO >=900
074700080529      *
074800080529      * comunque deve far vedere che è arrivato un legame x un Annullato
074900080529     c                   If        dpoANN <> *blank
075000080529     c                   clear                   dpoANN
075100080529     c                   end
075200061017      *
075300061017     C* aggancia l'ORM legato alla Bolla
075400061017     c                   eval      ar4trc ='M'
075500061017     c                   move      dpoPOE        dpoPOEa           3
075600061017     c                   move      dpoNRS        dpoNRSa           2
075700061017     c                   move      dpoNOR        dpoNORa           7
075800061017     c                   move      dpoNRV        dpoNRVa           2
075900070612     c****               eval      ar4n30 = %trim(dpopoeA) + %trim(dponrsA) +
076000070612     c****                                  %trim(dponorA) + %trim(dponrvA)
076100070612     c                   move      dpopoeA       §B4POE
076200070612     c                   move      dponrsA       §B4NSR
076300070612     c                   move      dponorA       §B4NOR
076400070612     c                   move      dponrvA       §B4NRV
076500070612     c                   eval      ar4n30 = dsbl4m
076600070613
076700070613     c                   move      'N'           Sped_xOrm         1
076800061017     C     Kar43         chain     fiar403l
076900070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
077000070613     c                   dow       not %Eof(fiar403l)
077100070613     c                   movel     AR4NOT        dsbl4M
077200070613     c                   if        §B4CLG = 'S'
077300070613     c                   move      'S'           Sped_xOrm         1
077400070613     c                   leave
077500070613     c                   end
077600070613     C     Kar43         reade     fiar403l
077700070613     c                   enddo
077800070613      ***
077900061017      * ?Se non trovato l'ORM legato ad una bolla invio l'errore
078000070613     c*********          if        not %Found(fiar403l)
078100070613     c                   if        Sped_xOrm = 'N'
078200061017     c                   movel     '01'          DPOCODER
078300061017     c                   else
078400061017      * ?se trovato, altro tipo di controllo sul Parcel:
078500061017      * ? La bolla generata potrebbe essere stata associata all'ORM manualmente
078600061017      * ? attribuendo UN ALTRO N.PARCEL  -->  SI DEVE SEGNALARE L'ANOMALIA.
078700061017     c                   eval      ar4trc ='I'
078800061017     C     Kar41         chain     fiar401l
078900061017     c                   if         %Found(fiar401l)
079000061017     c                   movel     ar4not        dsbl4i
079100061017
079200061017     c                   if        Lung_Parcel = 11
079300061017     C                   movel(p)  parcel11      alfa14           14
079400061017     c                   end
079500061017     c                   if        Lung_Parcel = 14
079600061017     C                   movel(p)  parcel14      alfa14
079700061017     c                   end
079800061017      * ?se Parcel diverso --> segnala l'errore
079900061017     C                   if        §B4IPN  <>  alfa14
080000061017     c                   movel     '02'          DPOCODER
080100061017     c                   end
080200061017     c                   end
080300061017     c                   end
080400061017      *
080500061017     c                   else
080600061017      * ?Se non era nella fase che si aspettava imposta il codice di errore --> '01'
080700061017      * ?   sul nostro avanzamento ORM per fase non congrua.
080800080523     c***   Se sono qui l'ho già ricevuto precedentemente e non devo + registrarlo
080900080523     c*********          movel     '01'          DPOCODER
081000060920     c                   end
081100061017     c                   endIF
081200060920      **
081300060920      *?      Ritirato Parcel in DPD
081400060920     c                   when      STATUS = '2'
081500060920
081600060920     c                   if        ORmFAO = 300 or ORmFAO = 400
081700080516      **
081800080516      ** Attenzione se si tratta di un ORM di un Parcel che deve essere consegnato
081900080516      **   all'Estero il nostro ORM deve essere chiuso con fase 910 (+82 ritirato da altra filiale)
082000080516     c                   if        ormNAC <> *blank                             All'Estero
082100080516     c                   z-add     910           ormFAO
082200080516     c                   else
082300080516
082400080516      ** Se consegna in Italia:
082500060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
082600060920     c                   z-add     600           ormFAO
082700080516      **
082800080516     c                   end
082900080516      **
083000060921     c                   exsr      aggior_fase
083100060921      *
083200060920     c                   else
083300080527      *
083400080527      * ? Se NON era stato ritirato (Fase 500) ma successivamente arriva
083500080527      * ? che è stato ritirato, si devono scrivere 2 fasi:
083600080527      * ? 400 -> Assegnato (nuovamente)
083700080527      * ? 600 -> Ritirato
083800080527     c                   if        ORmFAO =500
083900080527      *   Assegna
084000080605     c                   z-add     400           new_ormFAO
084100080527     c                   exsr      aggior_fase
084200080527      *   Ritirato
084300080605     c                   clear                   new_ormFAO
084400080527     c                   z-add     600           ormFAO
084500080527     c                   exsr      aggior_fase
084600080527      *
084700080527     c                   else
084800061017      *
084900061017      * ?- Attenzione: occorre controllare se c'è stato un problema di ritardo dell'informazione
085000061017      * ?  di stato ricevuta in ritardo rispetto al pacco che può essere già arrivato come bolla
085100061017      * ?  import e quindi l'ORM può trovarsi già chiuso.
085200061017      * ? MA TUTTO è già stato trattato operativamente in modo corretto (NON DEVE SEGNALARE NULLA)
085300061017      * ?altrimenti
085400061017      * ? SE COSI'non fosse, occorre segnalare l'errore x essere preso in considerazione.
085500061017     c                   if        ORmFAO >=900
085600061017      *
085700061017     C* aggancia l'ORM legato alla Bolla
085800061017     c                   eval      ar4trc ='M'
085900061017     c                   move      dpoPOE        dpoPOEa           3
086000061017     c                   move      dpoNRS        dpoNRSa           2
086100061017     c                   move      dpoNOR        dpoNORa           7
086200061017     c                   move      dpoNRV        dpoNRVa           2
086300070613     c*****              eval      ar4n30 = %trim(dpopoeA) + %trim(dponrsA) +
086400070613     c*****                                 %trim(dponorA) + %trim(dponrvA)
086500070613     c                   move      dpopoeA       §B4POE
086600070613     c                   move      dponrsA       §B4NSR
086700070613     c                   move      dponorA       §B4NOR
086800070613     c                   move      dponrvA       §B4NRV
086900070613     c                   eval      ar4n30 = dsbl4m
087000070613     c                   move      'N'           Sped_xOrm         1
087100070613     C     Kar43         chain     fiar403l
087200070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
087300070613     c                   dow       not %Eof(fiar403l)
087400070613     c                   movel     AR4NOT        dsbl4M
087500070613     c                   if        §B4CLG = 'S'
087600070613     c                   move      'S'           Sped_xOrm         1
087700070613     c                   leave
087800070613     c                   end
087900070613     C     Kar43         reade     fiar403l
088000070613     c                   enddo
088100070613      ***
088200061017      * ?Se non trovato l'ORM legato ad una bolla invio l'errore
088300070613     c********           if        not %Found(fiar403l)
088400070613     c                   if        Sped_xOrm = 'N'
088500061017     c                   movel     '01'          DPOCODER
088600061017     c                   else
088700061017      * ?se trovato, altro tipo di controllo sul Parcel:
088800061017      * ? La bolla generata potrebbe essere stata associata all'ORM manualmente
088900061017      * ? attribuendo UN ALTRO N.PARCEL  -->  SI DEVE SEGNALARE L'ANOMALIA.
089000061017     c                   eval      ar4trc ='I'
089100061017     C     Kar41         chain     fiar401l
089200061017     c                   if         %Found(fiar401l)
089300061017     c                   movel     ar4not        dsbl4i
089400061017
089500061017     c                   if        Lung_Parcel = 11
089600061017     C                   movel(p)  parcel11      alfa14           14
089700061017     c                   end
089800061017     c                   if        Lung_Parcel = 14
089900061017     C                   movel(p)  parcel14      alfa14
090000061017     c                   end
090100061017      * ?se Parcel diverso --> segnala l'errore
090200061017     C                   if        §B4IPN  <>  alfa14
090300061017     c                   movel     '02'          DPOCODER
090400061017     c                   end
090500061017     c                   end
090600061017     c                   end
090700061017      *
090800061017     c                   else
090900061017      * ?Se non era nella fase che si aspettava imposta il codice di errore --> '01'
091000061017      * ?   sul nostro avanzamento ORM per fase non congrua.
091100061017     c                   movel     '01'          DPOCODER
091200061017     c                   end
091300080527      *
091400080527     c                   end
091500080527      *
091600061017     c                   endIF
091700060920      **
091800060920      *?     Stornato l'Ordine in DPD
091900060920     c                   when      STATUS = '3'
092000060920
092100060920     c                   if        ORmFAO <= 300
092200060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
092300060920     c                   z-add     999           ormFAO
092400060921     c                   exsr      aggior_fase
092500060921      *
092600060920     c                   else
092700060920      **  Se non era nella fase che si aspettava
092800060920      **   imposta il codice di errore --> '03' a ricordare lo STATUS 3 come errore
092900061214     c********           movel     '03'          DPOCODER
093000060920     c                   end
093100060920      **
093200060920      *?      Mancato Ritiro + GLI SCAN + COD.ADD.
093300060920     c                   when      STATUS = '4'
093400061120
093500080407      *   Su Chiarimento con DPD: ?
093600080516      * ?  Si è deciso di rilevare il motivo dallo Status 4 qui oppure dal STATUSDATACR
093700080516      * ?   con lo SCAN 18 e l'exception code.
093800080516      * ?  Comunque il primo che riceviamo con il motivo lo prendiamo.    ?
093900080407      *
094000061120      ** attenzione : se si tratta di un Mancato ritiro xchè presenti + parcel rispetto
094100061120      **   a quelli previsti (quindi SCAN 18 con cod.add=82) significa che occorre far fare
094200061120      **   al committente altri ORM per poter ritirare i parcel in più.
094300061120      **
094400061120      ** i dati arrivati in questo modo significa che è stato abbinato un parcel all'ORM
094500061120      **  ma ce ne sono 2 che non hanno l'ORM e che non possono essere ritirati.
094600061120      **   0163;409990;0148;01489000002045;20061109;;1;;;;;;
094700061120      **   0163;409990;0148;01489000002045;20061109;;4;;;;18;082;
094800061120      **   0163;409990;0148;01489000002045;20061109;;4;;;;18;082;
094900061120      **  NOTARE lo stesso riferimento ordine e lo stesso nr.Parcel nei records status (4)
095000080407     c*
095100080404     c                   if        zcodeX =  Parcel_inPIU
095200061120     c
095300080404     c                   movel     Parcel_inPIU  DPOCODER
095400061120     c                   add       1             DPOMANC
095500061120     c                   move      DPOSTS        STATUS
095600061120
095700061120     c                   else
095800060920
095900080527     c******             if        ORmFAO <= 400
096000080527     c                   if        ORmFAO <= 500
096100080527
096200060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
096300080516     c******             z-add     999           ormFAO
096400080516     c***  da tabella MRD:
096500080829     c                   if        manFase <> 0
096600080829
096700080516     c                   z-add     manFase       ormFAO
096800060921     c                   exsr      aggior_fase
096900080829      **
097000080829     c                   else
097100080829      **
097200080829      **  Se segnalano un Mancato ritiro NON codificato lo segnalo e non
097300080829      **   aggiorno l'ORM.
097400080829     c                   movel     '03'          DPOCODER
097500080829      **
097600080829     c                   end
097700060920      **
097800060920     c                   else
097900060920      **  Se non era nella fase che si aspettava imposta il codice di errore --> '01'
098000060920      **    sul nostro avanzamento ORM per fase non congrua.
098100060920     c                   movel     '01'          DPOCODER
098200060920     c                   end
098300060920      **
098400061120     c                   end
098500061120      **
098600060920      *?      Cancellazione non possibile in DPD
098700060920     c                   when      STATUS = '5'
098800060920
098900100723     c                   if        ORmFAO >  900
099000080513      *     rilascia record per poterlo poi riaprire
099100080513     c                   update    fnORM000
099200080513      *
099300060920      ** deve riaprire l'ORM che era stato precedentemente chiuso
099400080423      ** con Fase 100.
099500080423     c                   clear                   fior51ds
099600080423     c                   eval      I51TIP = 'A'
099700080423     c                   eval      I51POE = dpoPOE
099800080423     c                   eval      I51NSR = dpoNRS
099900080423     c                   eval      I51NOR = dpoNOR
100000080423     c                   eval      I51NRV = dpoNRV
100100080423     c                   eval      I51FAR = 100
100200080423     c                   call      'FIOR51R'
100300080423     c                   parm                    fior51ds
100400080513      **
100500080513      **  Deve riattivare il record se annullato sul FIDPO
100600080513     c                   if        O51ERR = *blank and
100700080513     c                             dpoANN <> *blank
100800080513     c                   clear                   dpoANN
100900080513     c                   end
101000060920      **
101100060921     c                   else
101200060920      **  Se non era nella fase che si aspettava imposta il codice di errore --> '01'
101300060920      **    sul nostro avanzamento ORM per fase non congrua.
101400060920     c                   movel     '01'          DPOCODER
101500060920     c                   end
101600060920      **
101700060920     c                   endsl
101800080516      *?     Finiti gli STATUS:
101900060920      **
102000060920     c                   end
102100060920      **
102200060920      **  Quindi aggiorna FIDPO con o senza l'errore con il Nr.Parcel
102300060920      **   solo se non è stato precedentemente ricevuto e aggiornato dal file
102400060920      **    degli STATI con SCAN 18
102500080516     c                   if        dpoSTS <> 8  or
102600080516     c                             dpoSTS =  8 and DPOcdmr = *blank
102700080516      **
102800060920     c                   move      *zeros        DPOSTS
102900060920     c                   move      STATUS        DPOSTS
103000080516      **
103100061002      * se mancato ritiro
103200080404     c                   if        dpoSTS =  4 and zcodeX > 0
103300080404     c                   move      zcodeX        DPOcdmr
103400061002     c                   end
103500080516      **
103600060920     c                   z-add     data_oggi     DPODTRIC
103700060920     c                   end
103800080516      **
103900080530      **  Se deve segnalare un errore riattiva il
104000080530     c                   if        DPOCODER <> *blank
104100080530     c                   MOVEL     *blank        dpoANN
104200080530     c                   end
104300080530      **
104400060920     c                   MOVEL     PKNR          DPONRP
104500060920     c                   update    fidpo000
104501170102     c
104502170102     c* Richiamo pgm per aggiornare codice bolla da franco a ass su blp se ancora "Da partire"
104503170102     c                   clear                   fieu39ds
104504170102     c                   eval      I39POE=dpopoe
104505170102     c                   eval      I39NSR=dponrs
104506170102     c                   eval      I39NOR=dponor
104507170102     c                   eval      I39NRV=dponrv
104508170102     c                   eval      I39NPR=dponrp
104510170102     c                   call      'FIEU39R'
104511170102     c                   parm                    fieu39ds
104600060920      **
104700060920     c                   end
104800060809      * ?---------------------------------  **
104900060809      **
105000060809     c                   Endsr
105100060612      * ?------------------------------------------------------------------ */
105200060921      *?      Aggiorna Fase sugli ORM
105300060612      * ?------------------------------------------------------------------ */
105400060921     c     Aggior_Fase   Begsr
105500060612      **
105600080605      * per poter gestire la scrittura delle 2 fasi consecutive
105700080605      *  e aggiornare una volta sola l'FNORM000
105800080605      **
105900080605     c                   if        new_ormFAO = 0
106000080605      **
106100060921     c                   z-add     data_oggi     ormDFO
106200060921     c                   z-add     ore           ormOFO
106300060921     c                   update    fnORM000
106400080605      **
106500080605     c                   else
106600080605     c                   eval      ormFAO = new_ormFAO
106700080605     c                   end
106800060921
106900060921      **  Allinea le Fasi in dettaglio
107000060921     c     KOrM1         setgt     fnOrf01l
107100060921     c     KOrM1         readpe    fnOrf01l
107200060921     c                   if        not %Eof(fnorf01l)
107300080516      *
107400080516      ** Chiuso prima dell'affidamento
107500061017     c                   if           STATUS = '3'
107600061017     c                   eval         ORFCAR = '80'
107700060921     c                   end
107800080516      *
107900080516      ** Mancato Ritiro con motivo: (tabella "MRD")
108000061017     c                   if           STATUS = '4'
108100061017     c                   eval         ORFCAR = mancod
108200060921     c                   end
108300080516      **
108400080605      **  se prima era come mancato ritiro adesso riassegna e non deve
108500080605      **   impostare il codice di mancato ritiro.
108600080605     c                   if           STATUS = '2'
108700080605     c                   if        new_ormFAO = 400 and ORFFAR = 500
108800080605     c                   clear                   ORFCAR
108900080605     c                   end
109000080605     c                   end
109100080605      **
109200080516      **   Imposta Stessa Fase dell'ORM
109300060921     c                   z-add     ormFAO        ORFFAR
109400080516      **
109500080516      *  Quando Fase 910:
109600080516      *  è il caso di un ORM che deve essere ritirato e consegnato NON in ITALIA
109700080516      **  ma solo all'estero.
109800080516     c                   if        ormFAO = 910
109900080516     c                   eval      ORFCAR = Consegna_estero
110000080516     c                   end
110100080605      **
110200060921     c                   z-add     data_oggi     ORFDAE
110300060921     c                   z-add     data_oggi     ORFDFC
110400060921     c                   z-add     ore           ORFORE
110500060921     c                   eval      ORFPUE = 'ORMDPD_AUT'
110600060921     c                   write     fnorf000
110700060921     c                   end
110800060921      **
110900060921     c                   Endsr
111000060921      * ?------------------------------------------------------------------ */
111100060921      *?      Suddivide i campi della riga con carattere divisorio (;)
111200060921      * ?------------------------------------------------------------------ */
111300060921     c     Split_Rec     Begsr
111400060921      **
111500060612     c                   clear                   Nr_campi
111600060612     c                   clear                   Campi_Record
111700060612     c                   clear                   Campi_Lunghi
111800060612     c                   clear                   Campi_Numerici
111900060612     c                   clear                   Campi_Decimali
112000060612     c                   movel     ';'           Separa_campi
112100090302     c                   movel     '.'           Decimal_Separ
112200060612      *
112300060612     c                   call      'FIEU00R'
112400060612      * ?     Input
112500060612     c                   parm                    Separa_campi
112600060612     c                   parm                    Decimal_Separ
112700060612     c                   parm                    dati
112800060612      * ?     Output  su schiere di 100 elementi
112900060612     c                   parm                    Nr_campi
113000060612     c                   parm                    Campi_Record                   * i dati
113100060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
113200060612     c                   parm                    Campi_Numerici                 * se numerici
113300060612     c                   parm                    Campi_Decimali                 * quanti decimali
113400060612      **
113500060612     c                   Endsr
113600971215      *----------------------------------------------------------------
113700051205      *   DEFINIZIONE CHIAVI                               *
113800060808      *----------------------------------------------------------------
113900051205     C     *INZSR        BEGSR
114000051205      *
114100991129     c     *ENTRY        PLIST
114200060613     C                   parm                    esito             1
114300060809      **
114400060809     c     Korm1         klist
114500060809     c                   kfld                    DPOPOE
114600060809     c                   kfld                    DPONRS
114700060809     c                   kfld                    DPONOR
114800060809     c                   kfld                    DPONRV
114900061017      *
115000061017     C     Kar43         KLIST
115100061017     C                   KFLD                    ar4trc
115200061017     C                   KFLD                    ar4n30
115300061017      *
115400061017     c     kar41         klist
115500061017     C                   kfld                    AR4AAS
115600061017     C                   kfld                    AR4LNP
115700061017     C                   kfld                    AR4NRS
115800061017     C                   kfld                    AR4NSP
115900061017     C                   kfld                    ar4trc
116000060809      **
116100061017     c     KDPO3         klist
116200060809     c                   kfld                    ODEPOT
116300060809     c                   kfld                    ORDERNR
116400060808      *
116500060808      *  imposta UDATE di elaborazione
116600060915     C                   move      *date         data_oggi
116700060915     C                   move      *date         G02inv
116800060915     C                   MOVE      *ZEROS        G02dat
116900060808     C                   MOVE      *BLANKS       G02ERR
117000060808     C                   CALL      'XSRDA8'
117100060808     C                   PARM                    WLBDAT
117200060808     C                   move      data_oggi     day
117300060808     C                   TIME                    W0140
117400060808     C                   MOVEl     W0140         ore
117500060906
117600060906      **?carica motivi mancata consegna                                     */
117700060906     c     'MRD'         setll     tntbe01l
117800060906     c                   do        *hival
117900060906     c     'MRD'         reade     tntbe01l
118000060906     c                   if        %eof(tntbe01l)
118100060906     c                   leave
118200060906     c                   endif
118300060906     c                   movel     tbeuni        dmrd
118400060906     c                   add       1             x
118500060906     c                   movel     tbeke1        manb(x)
118600060906     c                   movel     §mrdcar       mand(x)
118700060906     c                   movel     §mrdfar       manf(x)
118800060906     c                   enddo
118900060906
119000050414      *
119100991124     C                   ENDSR
119200060621      * ?------------------------------------------------------------------ */
119300060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
119400060621      * ?------------------------------------------------------------------ */
119500060621     C     *pssr         BEGSR
119600060621     C
119700060710     C                   eval      esito = '2'
119800060621     C
119900060621     C                   ENDSR     '*CANCL'
120000060614      *-------------------------------------------------------------------------
