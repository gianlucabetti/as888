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
001400171019     F*****fiar401l  if   E           k disk    rename(FIAR4000:FIAR4001)
001500171019     Ffipnd01l  if   E           k disk
001600061017     Ffiar403l  if   E           k disk
001700060906     FTntbe01L  iF   E           k DISK
001800060808      * ?================================================================== */
001900060710     D Parcel14        S             14
002000060809     D Tipo_error      S              1  0
002100080516     D Consegna_estero...
002200080516     D                 S              2    INZ('82')
002300060808      * ?================================================================== */
002400060808     D  data_Oggi      s              8  0 INZ(0)
002500060808     D  day            s              8  0 INZ(0)
002600060808     D  ore            s              6  0 INZ(0)
002700060808     D  w0140          s             14  0 INZ(0)
002800060809      *
002900060808     D WLBDAT          DS
003000060808     D  G02DAT                 1      8  0
003100060808     D  G02INV                 9     16  0
003200060808     D  G02ERR                17     17
003300060808     D  G02TGI                18     22  0
003400060906     D x               s              3s 0
003500060906     D mancod          s              2
003600060906     D manfase         s              3s 0
003700060906     D manb            s              2    dim(50)
003800060906     D mand            s              3s 0 dim(50)
003900060906     D manf            s              3s 0 dim(50)
004000170228      *-------------------
004100170228     d dfar          e ds
004200170228      *-------------------
004300060906     D dmrd          E DS
004400171019     D**** dsbl4I        E DS
004500070612     D dsbl4M        E DS
004600060809      * ?   *--------------------------------------------------------------*
004700060809      * ?   ( Descrizione Campi x scomposizione FLAT record )
004800060809      * ?   *--------------------------------------------------------------*
004900060809     D FIDPODTA2     e DS
005000061002     D     Zcode5             61     63  0
005100061002     D     Zcode4             64     66  0
005200060809     D     Zcode3             67     69  0
005300061002     D     Zcode2             70     72  0
005400061002     D     Zcode1             73     75  0
005500080404     D     Zcod               61     75  0 DIM(5)
005600080404      *
005700080404      * ?( add.code ==> [82] Parcel in più )
005800080404     D Parcel_inPIU    s              3s 0 inz(82)
005900080404      *
006000080423      * ?   *--------------------------------------------------------------*
006100080423      *         per riapertura automatica dell'ORM  --> FIOR51R
006200080423      * ?   *--------------------------------------------------------------*
006300080423     D FIOR51DS      e DS
006400060612      * ?================================================================== */
006500170102     d FIEU39DS      e DS
006600170102
006700060809      * ?   * Campi da tradurre con interi
006800060809     D atoll           PR            20I 0
006900060809     D                                     EXTPROC('atoll')
007000060809     D  Char                           *
007100060809     D                                     VALUE
007200060809     D                                     OPTIONS(*STRING)
007300060612      * ?   * Campi da tradurre con interi
007400060612     D atoi            PR            10I 0
007500060612     D                                     EXTPROC('atoi')
007600060612     D  Char                           *
007700060612     D                                     VALUE
007800060612     D                                     OPTIONS(*STRING)
007900060612      * ?   * Campi da tradurre con decimali
008000060612     D atof            PR             8F
008100060612     D                                     EXTPROC('atof')
008200060612     D  Char                           *
008300060612     D                                     VALUE
008400060612     D                                     OPTIONS(*STRING)
008500060612      * ?   *--------------------------------------------------------------*
008600060612      * ?   ( Descrizione Campi x scomposizione FLAT record )
008700060612      * ?   *--------------------------------------------------------------*
008800060612      *
008900060612      * ?   * Campi x decodifica * (INPUT  del Record)
009000060612     D  Dati           s           1500
009100060612     D Separa_campi    s              1
009200060612     D Decimal_Separ   s              1
009300060809      *
009400060612      * ?   * Campi decodificati * (OUTPUT del Record)
009500060612      *       Schiere di Output / Campi di Output
009600060614     D  Alfa40         s             40
009700060612     d  Sk             s              3u 0
009800060612     D    NR_campi     s              3u 0
009900060612     D  Campi_Record   ds
010000060612     D    Dato_sk                    40    DIM(100)
010100060612     D  Campi_Lunghi   ds
010200060612     D    LunDato_sk                  3u 0 DIM(100)
010300060612     D Campi_Numerici  ds
010400060612     D    DatoNum_sk                  1    DIM(100)
010500060612     D Campi_Decimali  ds
010600060612     D    Decimal_sk                  3u 0 DIM(100)
010700060612      * ?   *--------------------------------------------------------------*
010800060612     D  position       s              3  0 INZ(0)
010900060612
011000060612     D  se_errore      s              1    inz(' ')
011100060612     D  Msg_Err        s            132    inz(' ')
011200060614     D  Msg_Vin_80     s             80    inz(' ')
011300060612      * ?* ------------------------------------------------------ *
011400060612      * Records di controllo mandati nell'IGATEAVIS
011500060612      * ?   * Dati Record UNO *
011600060612     DDepot_Trs_UNO    s             10
011700060612     D User_Trs_UNO    s             10
011800060612     D Data_Trs_UNO    s              8  0
011900060612     D Time_Trs_UNO    s              6  0
012000060612     D  Num_Trs_UNO    s              9  0
012100060809      * ?   * Dati Record DUE *
012200060809     D Vers_Trs_DUE    s             15
012300060809      * ?   * Dati Record TRE *
012400060612     D Testa_Sk        s             10    DIM(50)
012500060809      * ?   * Dati Record QUATTRO *
012600060612     D  Num_Trs_FIN    s              9  0
012700060710      **
012800060710     D Digits          C                   '0123456789'
012900060612      * ?================================================================== */
013000060612      *   Ciclo principale
013100060413      * ?********************************************************************/
013200060612      **  da TIVIN00R esegue la pretraduzione portando su DDS ogni record
013300060612     C*
013400060612     C                   if        not %open(tivin00r)
013500060612     C                   open      tivin00r
013600060612     C                   endif
013700060612      **
013800060612      * ?------------------------------------------------------------------ */
013900060612     C*? Controllo dati arrivati da DPD
014000060612      * ?------------------------------------------------------------------ */
014100060612      * ?- Occorre fare un primo test sull'integrità della trasmissione
014200060612      * ?- controllando che la trasmissione sia completa.
014300060612
014400060612      * ?              /*---------------------- */
014500060612     c                   exsr      check_Trasm
014600060612      * ?              /*---------------------- */
014700060612      **
014800060613      **  Errore di trasmissione x tutti i records
014900060613      **   --> file in errore
015000060613     c                   if        se_errore = 'S'
015100060612
015200060612      ** Messaggio da riportare su ogni record x tutta la trasmissione
015300060612     C                   eval      Msg_Err = 'MSG ricevuto INCOMPLETO +
015400060613     C                               >> Farsi rimandare i DATI !!'
015500060612
015600060612      * ?  Scrive su tutti i records il tipo di errore
015700060612     c     *start        setll     tivin00r
015800060612     c                   read      tivin00r
015900060612     c                   dow       not %eof(tivin00r)
016000060612     C                   eval      vinMSG = Msg_Err
016100060612     C                   eval      vinFLG = '2'
016200060613     c                   eval      esito  = '2'
016300060612     c                   update    tivin000
016400060612     c                   read      tivin00r
016500060612     c                   endDO
016600060612      **
016700060612     c                   else
016800060614      **
016900060612      * ?------------------------------------------------------------------ */
017000060612     C*? Se TUTTO OK esegue l'importazione delle Bolle  controllando i campi se OK.
017100060612      * ?------------------------------------------------------------------ */
017200060612      * ?              /*---------------------- */
017300060612     c                   exsr      Importa_Msg
017400060612      * ?              /*---------------------- */
017500060612
017600060614     c                   end
017700060614
017800060612     C                   if        %open(tivin00r)
017900060612     C                   close     tivin00r
018000060612     C                   endif
018100060612      *
018200060614      *  se c'erano errori bloccanti ma almeno un record è stato tradotto
018300060614      *
018400060710     c                   if        almeno_uno ='S' and esito ='1'
018500060710     C                   eval      esito ='0'
018600060614     C                   endif
018700170102
018800170102     c                   eval      i39tla='C'
018900170102     c                   call      'FIEU39R'
019000170102     c                   parm                    fieu39ds
019100060614      *
019200060612     c                   SETON                                        LR
019300060612      * ?================================================================== */
019400060612      *? Controlla la trasmissione   se completa
019500060612      * ?------------------------------------------------------------------ */
019600060612     c     Check_Trasm   Begsr
019700060612
019800060612     C                   clear                   se_errore
019900060612      ** primo  record
020000060612     c     *start        setll     tivin00r
020100060612     c                   read      tivin00r
020200060612     c                   if        not %eof(tivin00r)
020300060612      * ?              /*---------------------- */
020400060612     c                   exsr      Decod_Record
020500060612      * ?              /*---------------------- */
020600060612     c                   endif
020700060612      **
020800060612      ** ultimo record
020900060612     c     *hival        setll     tivin00r
021000060612     c                   readp     tivin00r
021100060612     c                   if        not %eof(tivin00r)
021200060612      * ?              /*---------------------- */
021300060612     c                   exsr      Decod_Record
021400060612      * ?              /*---------------------- */
021500060612     c                   endif
021600060612
021700060612      * ?    Se l'inizio e la fine trasmissione non coincidono ossia NON hanno
021800060612      * ?    lo stesso numero trasmissione allora si deve segnalare l'errore
021900060612      * ?    e impostare tutto il file sul file degli errori come MSG INCOMPLETO.
022000060612     C                   if        num_trs_UNO <> num_trs_FIN
022100060612      * ?-----> Errore
022200060612     C                   eval      se_errore = 'S'
022300060612
022400060612     c                   end
022500060612
022600060612     c                   endSR
022700060612      * ?------------------------------------------------------------------ */
022800060612      *? Importa i records della tramsissione
022900060612      * ?------------------------------------------------------------------ */
023000060612     c     Importa_Msg   Begsr
023100060612
023200060614      * per controllare se almeno un record è stato importato sul VAB
023300060614     c                   clear                   Almeno_Uno        1
023400060614
023500060612     c     *start        setll     Tivin00r
023600060612     c                   read      Tivin00r
023700060612
023800060612     c                   dow       not %eof(Tivin00r)
023900060614
024000060614      * solo i record sflaggati da rielaborare
024100060614     c                   IF        vinFLG = *blank and vinDTA <> *blank
024200060612
024300060612      ** Controlli formali sui campi
024400060612     c                   clear                   se_errore
024500060620     C                   clear                   Msg_Vin_80
024600060612
024700060612      ** Decodifica record a campi variabili
024800060612      * ?              /*---------------------- */
024900060612     c                   exsr      Decod_Record
025000060612      * ?              /*---------------------- */
025100060612
025200060612      **  Se presente un errore nel record emette una segnalazione msg
025300060612     c                   if        se_errore ='S'
025400060614     C                   eval      vinMSG = Msg_Vin_80
025500060621     c                   end
025600060612
025700060809      *  con il record in canna scrive direttamente il VAO
025800060620     c                   if        %subst(dati:1:1) <> '#'
025900060612      * ?              /*---------------------- */
026000060809     c                   exsr      WRT_FILE
026100060612      * ?              /*---------------------- */
026200060620     c                   end
026300060614
026400060809      *  Problemi nella decodifica dei campi VAO
026500060614     c                   if        se_errore ='S'
026600060614     C                   eval      vinMSG = Msg_Vin_80
026700060614     c                   end
026800060612
026900060621      *  Sempre Record OK
027000060621     C                   eval      vinFLG = '1'
027100060710     c                   eval      esito  = '0'
027200060621
027300060809      *  x errore bloccante nella composizione del VAO
027400060621     c                   if        err_bloccante ='S'
027500060621     C                   eval      vinFLG = '2'
027600060710     c                   eval      esito  = '1'
027700060621     c                   end
027800060612
027900060612     c                   update    Tivin000
028000060614     c                   endIF
028100060612
028200060612     c                   read      Tivin00r
028300060612     C                   ENDdo
028400060612      **
028500060612     c                   endSR
028600060612      * ?------------------------------------------------------------------ */
028700060612      *?    Decodifica record a campi variabili
028800060612      * ?------------------------------------------------------------------ */
028900060612     c     Decod_Record  Begsr
029000060612
029100060612      * ? Sposta il record a tracciato libero su un campo di lavoro       =
029200060612     c                   movel(p)  VINDTA        dati
029300060612      **
029400060612      * ?              /*---------------------- */
029500060612     c                   exsr      Split_Rec
029600060612      * ?              /*---------------------- */
029700060612      **
029800060612      **  Records di Intestazione e di fine Msg
029900060612     c                   if        %subst(dati:1:1) ='#'
030000060612      * ?              /*---------------------- */
030100060612     c                   exsr      Rec_Speciali
030200060612      * ?              /*---------------------- */
030300060612     c                   Else
030400060612      ** conta le righe
030500060612      * ?              /*---------------------- */
030600060612     c                   exsr      Rec_Dettaglio
030700060612      * ?              /*---------------------- */
030800060612     c                   End
030900060612      **
031000060612     c                   Endsr
031100060612      * ?------------------------------------------------------------------ */
031200060612      *?      Record di Testata e di Fine Msg
031300060612      * ?------------------------------------------------------------------ */
031400060612     c     Rec_Speciali  Begsr
031500060612      **
031600060612      * ?  Prima riga
031700060612     c     'FILE'        SCAN      dati:2        position
031800060612     c                   if        %Found and position < 5
031900060612     c                   exsr      Rec_UNO
032000060612     c                   ElSe
032100060612     c                   End
032200060612      **
032300060612      * ?  Seconda riga (testate dei campi)
032400060809     c     'Version'     SCAN      dati:2        position
032500060809     c                   if        %Found and position < 5
032600060809     c                   exsr      Rec_DUE
032700060809     c                   End
032800060809      **
032900060809      * ?  Terza riga (testate dei campi)
033000060612     c     'DEF'         SCAN      dati:2        position
033100060612     c                   if        %Found and position < 5
033200060809     c                   exsr      Rec_TRE
033300060612     c                   End
033400060612      **
033500060612      * ?  Ultima riga
033600060612     c     'END'         SCAN      dati:2        position
033700060612     c                   if        %Found and position < 5
033800060612     c                   exsr      Rec_LAST
033900060612     c                   End
034000060612      **
034100060612     c                   Endsr
034200060612      * ?------------------------------------------------------------------ */
034300060612      *?      Primo Record
034400060612      * ?------------------------------------------------------------------ */
034500060612     c     Rec_UNO       Begsr
034600060612      **
034700060612      ** esempio di record :
034800060808      * ?   # FILE;N00;depot945;20050603;143526;6;
034900060612      **
035000060612      **  Inizializza i campi della trasmissione
035100060612     C                   eval      Depot_Trs_UNO = *blank
035200060612     C                   eval      User_Trs_UNO  = *blank
035300060612     C                   eval      Data_Trs_UNO  = 0
035400060612     C                   eval      Time_Trs_UNO  = 0
035500060612     C                   eval      Num_Trs_UNO   = 0
035600060612
035700060612      **   Memorizza il numero della trasm. da confrontare con l'ultimo record.
035800060612      **     Tutto il dettaglio deve essere compreso entro questi 2 records
035900060612      **    altrimenti il msg.non è completo e bisogna farsi rimandare il file.
036000060612
036100060808      * ? Nel 2° campo di testata c'è lo USER in Delis
036200060808     C                   z-add     2             sk
036300060808     c                   eval      User_Trs_UNO = %subst(Dato_sk(sk):1:+
036400060808     c                             LunDato_sk(sk))
036500060808
036600060808      * ? Nel 3° campo di testata c'è il Dispatch Depot
036700060808     C                   z-add     3             sk
036800060808     c                   eval      Depot_Trs_UNO = %subst(Dato_sk(sk):1:+
036900060808     c                             LunDato_sk(sk))
037000060808
037100060808      * ? Nel 4° campo di testata c'è la data trasmissione
037200060808     C                   z-add     4             sk
037300060808     c                   if        DatoNum_sk(sk) = 'S'
037400060808     c                   eval      Data_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
037500060808     c                   End
037600060808
037700060808      * ? Nel 5° campo di testata c'è il nr.seriale di trasmissione
037800060808     C                   z-add     5             sk
037900060808     c                   if        DatoNum_sk(sk) = 'S'
038000060808     c                   eval      Time_Trs_UNO = atoi(%TRIM(Dato_sk(sk)))
038100060808     c                   End
038200060808
038300060808      * ? Nel 6° campo di testata c'è il nr.seriale di trasmissione
038400060808     C                   z-add     6             sk
038500060808     c                   if        DatoNum_sk(sk) = 'S'
038600060808     c                   eval      num_trs_UNO = atoi(%TRIM(Dato_sk(sk)))
038700060808     c                   End
038800060612      **
038900060612     c                   Endsr
039000060809      * ?------------------------------------------------------------------ */
039100060809      *?      Secondo Record
039200060809      * ?------------------------------------------------------------------ */
039300060809     c     Rec_DUE       Begsr
039400060809      **
039500060809      ** esempio di record :
039600060809      * ?   # Version;JR20040831;
039700060809      **
039800060809      **  Inizializza i campi della versione trasmissione
039900060809     C                   eval      Vers_Trs_DUE = *blank
040000060809      **
040100060809      **   Memorizza la versione i trasm.
040200060809
040300060809      * ? Nel 2° campo di testata c'è lo USER in Delis
040400060809     C                   z-add     2             sk
040500060809     c                   eval      Vers_Trs_DUE = %subst(Dato_sk(sk):1:+
040600060809     c                             LunDato_sk(sk))
040700060809      **
040800060809     c                   Endsr
040900060612      * ?------------------------------------------------------------------ */
041000060809      *?      Terzo   Record
041100060612      * ?------------------------------------------------------------------ */
041200060809     c     Rec_TRE       Begsr
041300060612      **
041400060612      ** esempio di record :
041500060808      * ?   # DEF;CR:CRDATA01;ODEPOT;ORDERNR;CDEPOT;OCUSTNO;ONAME;ONAME2;OS...
041600060612      **
041700060612      **  Inizializza i campi della trasmissione (schiera con i nomi dei campi)
041800060612     C                   clear                   Testa_Sk
041900060612
042000060612      * ? Dal 3° campo troviamo i nomi delle intestazioni dei campi C.Request
042100060612     C     3             do        50            sk
042200060612     c                   eval      Testa_Sk(sk-2) = %subst(Dato_sk(sk):1:+
042300060612     c                             LunDato_sk(sk))
042400060612     c                   enddo
042500060612      **
042600060612     c                   Endsr
042700060612      * ?------------------------------------------------------------------ */
042800060612      *?      Ultimo  Record
042900060612      * ?------------------------------------------------------------------ */
043000060612     c     Rec_LAST      Begsr
043100060612      **
043200060612      ** esempio di record :
043300060808      * ?   # END;6;
043400060612      **
043500060612      **  Inizializza i campi della trasmissione
043600060612     C                   eval      Num_Trs_fin = 0
043700060612      **
043800060612      **  Nell'ultimo record di chiusura deve esserci lo stesso numero di trasmissione
043900060612      **  impostato nel primo record.
044000060612      **
044100060612      * ? Nel 2° campo di fine trsm. c'è il nr.seriale di trasmissione
044200060612     C                   z-add     2             sk
044300060612     c                   if        DatoNum_sk(sk) = 'S'
044400060612     c                   eval      num_trs_fin = atoi(%TRIM(Dato_sk(sk)))
044500060612     c                   End
044600060612      **
044700060612     c                   Endsr
044800060612      * ?------------------------------------------------------------------ */
044900060612      *?      Riga Bolla Import da controllare e tradurre
045000060612      * ?------------------------------------------------------------------ */
045100060612     c     Rec_Dettaglio Begsr
045200060612      **
045300060612      **   Controlli Formali sui campi: Obbligatorietà e controllo Numerico
045400060612      **
045500060809     c                   clear                   fidpoDTA2
045600060614     c                   clear                   Msg_Err
045700171019     c                   clear                   Parcel14
045800060809      **
045900060809      * ? Nel 1° campo : Numero Requesting Depot     (Oblligatorio)  ODEPOT
046000060809      **                                                             DEPOT - ORDINANTE
046100060809     C                   z-add     1             sk
046200060809     c                   If        LunDato_sk(sk) =  0
046300060809      * ?-----> Errore
046400060809     C                   eval      tipo_error = 1
046500060809     C                   exsr      Field_Error
046600060809     c                   Else
046700060809     c                   eval      ODEPOT = %subst(Dato_sk(sk):1:+
046800060809     c                                              LunDato_sk(sk))
046900060809     c                   End
047000060809      **
047100060809      * ? Nel 2° campo : Numero Ordine DPD           (Oblligatorio)  ORDERNR
047200060809      **   parte 2 della chiave ORM DPD                              NUM.ORDINE
047300060809     C                   add       1             sk
047400060809     c                   If        DatoNum_sk(sk) = 'N' or
047500060809     c                             LunDato_sk(sk) =  0
047600060809      * ?-----> Errore
047700060809     C                   eval      tipo_error = 2
047800060809     C                   exsr      Field_Error
047900060809     c                   Else
048000060809     c                   eval      ORDERNR= atoi(%TRIM(Dato_sk(sk)))
048100060809     c                   End
048200060809      **
048300060809      * ? Nel 3° campo : Depot Che deve eseguire ORM                 CDEPOT
048400060809      **                                                             DEPOT DI RITIRO
048500060809     C                   add       1             sk
048600060809     c                   If        LunDato_sk(sk) =  0
048700060809      * ?-----> Errore
048800060809     C                   eval      tipo_error = 1
048900060809     C                   exsr      Field_Error
049000060809     c                   Else
049100060809     c                   eval      RDEPOT = %subst(Dato_sk(sk):1:+
049200060809     c                                              LunDato_sk(sk))
049300060809     c                   End
049400060809      **
049500060809      * ? Nel 4° campo : Parcel Number                               PKNR
049600060809      **                                                             PARCEL NR
049700060809     C                   add       1             sk
049800060809     c                   If        LunDato_sk(sk) =  0
049900060809      * ?-----> Errore
050000060809     C                   eval      tipo_error = 1
050100060809     C                   exsr      Field_Error
050200060809     c                   Else
050300060809     c                   eval      PKNR   = %subst(Dato_sk(sk):1:+
050400060809     c                                              LunDato_sk(sk))
050500060809      * ?   ( Il parcel non è + numerico )   controlla lunghezza del Parcel
050600171019     C                   MOVEL     PKNR          parcel14         14
050700060809      **
050800060809     c                   End
050900090227      **
051000090227      * ? Nel 5° campo : Data Creazione Ordine                       NEUDAT
051100090227      **                                                             DATA CREAZIONE ORDINE
051200090227     C                   add       1             sk
051300090227     c                   If        DatoNum_sk(sk) = 'N' and
051400090227     c                             LunDato_sk(sk) >  0
051500090227      * ?-----> Errore
051600090227     C                   eval      tipo_error = 3
051700090227     C                   exsr      Field_Error
051800090227     c                   Else
051900090227     c                   eval      NEUDAT  =atoi(%TRIM(Dato_sk(sk)))
052000090227     c                   End
052100060809      **
052200090227      * ? Nel 6° campo : Data Ritiro                                 DATPCK
052300060809      **                                                             DATA RITIRO
052400060809     C                   add       1             sk
052500060809     c                   If        DatoNum_sk(sk) = 'N' and
052600060809     c                             LunDato_sk(sk) >  0
052700060809      * ?-----> Errore
052800060809     C                   eval      tipo_error = 3
052900060809     C                   exsr      Field_Error
053000060809     c                   Else
053100060809     c                   eval      DATPCK  =atoi(%TRIM(Dato_sk(sk)))
053200060809     c                   End
053300060809      **
053400060809      * ? Nel 7° campo : STATUS                      (Oblligatorio)  STATUS
053500060809      **                                                             status 1-2-3-4-5
053600060809     C                   add       1             sk
053700060809     c                   If        LunDato_sk(sk) =  0
053800060809      * ?-----> Errore
053900060809     C                   eval      tipo_error = 1
054000060809     C                   exsr      Field_Error
054100060809     c                   Else
054200060809     c                   eval      STATUS = %subst(Dato_sk(sk):1:+
054300060809     c                                              LunDato_sk(sk))
054400060809     c                   End
054500060809      **
054600060809      * ? Nel 8° campo : Peso (Con decimali)                         WEIGHT
054700060809      **                                                             PESO IN KG
054800060809     C                   add       1             sk
054900060809     c                   If        DatoNum_sk(sk) = 'N' and
055000060809     c                             LunDato_sk(sk) >  0
055100060809      * ?-----> Errore
055200060809     C                   eval      tipo_error = 3
055300060809     C                   exsr      Field_Error
055400060809     c                   Else
055500060809     c                   eval      WEIGHT = atof(%TRIM(Dato_sk(sk)))
055600060809     c                   End
055700060809      **
055800060809      * ? Nel 9° campo : SERVICE CODE                                SERVICE
055900060809      **                                                             SERVICE CODE
056000060809     C                   add       1             sk
056100060809     c                   If        DatoNum_sk(sk) = 'N' and
056200060809     c                             LunDato_sk(sk) >  0
056300060809      * ?-----> Errore
056400060809     C                   eval      tipo_error = 3
056500060809     C                   exsr      Field_Error
056600060809     c                   Else
056700060809     c                   eval      SERVICE= atoi(%TRIM(Dato_sk(sk)))
056800060809     c                   End
056900060809      **
057000060809      * ? Nel 10° campo : CUSTOM  status code                        CUSTOM
057100060809      **                                                             CUSTOM S.CODE
057200060809     C                   add       1             sk
057300060809     c                   If        LunDato_sk(sk) >  0
057400060809      * ?-----> Errore
057500060809     c                   eval      CUSTOM = %subst(Dato_sk(sk):1:+
057600060809     c                                              LunDato_sk(sk))
057700060809     c                   End
057800060809      **
057900060809      * ? Nel 11° campo : Codice Scan                                SCANT
058000060809      **                                                             SCAN
058100060809     C                   add       1             sk
058200060809     c                   If        DatoNum_sk(sk) = 'N' and
058300060809     c                             LunDato_sk(sk) >  0
058400060809      * ?-----> Errore
058500060809     C                   eval      tipo_error = 3
058600060809     C                   exsr      Field_Error
058700060809     c                   Else
058800060809     c                   eval      SCANT  = atoi(%TRIM(Dato_sk(sk)))
058900060809     c                   End
059000060809      **
059100060809      * ? Nel 12° campo : 5 Codici Addizionali                       ZCODE
059200060809      **                                                             COD.AGGIUNTIVI
059300060809     C                   add       1             sk
059400060809     c                   If        DatoNum_sk(sk) = 'N' and
059500060809     c                             LunDato_sk(sk) >  0
059600060809      * ?-----> Errore
059700060809     C                   eval      tipo_error = 3
059800060809     C                   exsr      Field_Error
059900060809     c                   Else
060000060809     c                   eval      ZCODE  = atoi(%TRIM(Dato_sk(sk)))
060100060809     c                   End
060200060808      **
060300060809      * ?----->  HA I CAMPI IMPOSTATI SULLA DS DEL FIDPODTA2
060400060612      **
060500060612     c                   Endsr
060600060614      * ?------------------------------------------------------------------ */
060700060614      *?      Campi in errore
060800060614      * ?------------------------------------------------------------------ */
060900060614     c     Field_Error   Begsr
061000060614      **
061100060614     C                   eval      se_errore   = 'S'
061200060614      * msg video o in posta
061300060614     c                   If        Msg_Err = *blank
061400060614     c
061500060614     c                   Select
061600060614     c
061700060614     c                   When      Tipo_Error = 1
061800060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
061900060614     C                             ' >> Obblig.- Manca Dato !!'
062000060614     c
062100060614     c                   When      Tipo_Error = 2
062200060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
062300060614     C                             ' >> Obblig.Mancante o NON Numerico !!'
062400060614     c
062500060614     c                   When      Tipo_Error = 3
062600060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
062700060614     C                             ' >> NON Numerico  !!'
062800060614     c
062900060614     c                   When      Tipo_Error = 4
063000060614     C                   eval      Msg_Err = 'Campo : ' + %trim(Testa_sk(sk)) +
063100060614     C                             ' >> Conversione non riuscita !!'
063200060614     c                   EndSL
063300060614     c
063400060614     c                   End
063500060614      * Errore su VINMSG
063600060614     C                   eval      Msg_Vin_80 = %trim(Msg_Vin_80)   + ';'
063700060614     c                                          + %trim(Testa_sk(sk))
063800060614      **
063900060614     c                   Endsr
064000060808      * ?------------------------------------------------------------------ */
064100060808      *?    Scrive i Files degli ORM
064200060808      * ?------------------------------------------------------------------ */
064300060809     c     WRT_FILE      Begsr
064400060809      **
064500060809      **  Se è in questo punto significa che non sono stati trovati errori
064600060809      **    formali quindi si passa gli status per avanzamento ORM
064700060809      **?              /*---------------------- */
064800060809     c                   ExSr      Avanza_ORM
064900060809      * ?              /*---------------------- */
065000060808      **
065100060809     c                   endSR
065200060809      * ?------------------------------------------------------------------ */
065300060809      *?    Imposta i campi per il VAO
065400060809      * ?------------------------------------------------------------------ */
065500060809     c     Avanza_ORM    Begsr
065600060809      **
065700060809     c                   clear                   err_bloccante     1
065800080605     c                   clear                   new_ormFAO        3 0
065900060906      * ?---------------------------------  **
066000060906      *?      ATTENZIONE: DA COMPLETARE.
066100060906      *?      COME GESTIRE I NUOVI STATUS
066200060906      *?       e GLI SCAN + COD.ADD.
066300060906      * ?---------------------------------  **
066400060906      **  SCAN
066500060906      **   SCAN + cod.addizionale
066600060906     c                   if        SCANT > 0
066700080404      **  ZCODE
066800061109     c                   clear                   mancod
066900061109     c                   clear                   manfase
067000080404     c                   clear                   ZcodeX            3 0
067100080404      *
067200080404     c                   if        ZCODE > 0
067300080404      *
067400080404      * ?/* subito controlla la presenza di un Parcel eccedente
067500080404      * ?   indicato con -->(82) */
067600080404     c                   z-add     1             x
067700080404     c     Parcel_inPIU  lookup    zcod(x)                                66
067800080404     c                   if        %Equal
067900080404     c                   z-add     Parcel_inPIU  zcode1
068000080404     c                   end
068100080404      *
068200080404      ** prendo in considerazione solo il primo valido
068300060906     c                   z-add     1             x
068400080404     c                   if        Zcode1 > 0
068500060906     c     zcode1        lookup    mand(x)                                66
068600080404     c   66              z-add     zcode1        ZcodeX
068700080404     c                   elseIf    Zcode2 > 0
068800080404     c     zcode2        lookup    mand(x)                                66
068900080404     c   66              z-add     zcode2        ZcodeX
069000080404     c                   elseIf    Zcode3 > 0
069100080404     c     zcode3        lookup    mand(x)                                66
069200080404     c   66              z-add     zcode3        ZcodeX
069300080404     c                   elseIf    Zcode4 > 0
069400080404     c     zcode4        lookup    mand(x)                                66
069500080404     c   66              z-add     zcode4        ZcodeX
069600080404     c                   elseIf    Zcode5 > 0
069700080404     c     zcode5        lookup    mand(x)                                66
069800080404     c   66              z-add     zcode5        ZcodeX
069900080404     c                   Endif
070000080404      *
070100080404     c                   if        %Equal
070200060906     c                   movel     manb(x)       mancod
070300060906     c                   movel     manf(x)       manfase
070400061109     c                   End
070500060906      **
070600060906     c                   End
070700060906      **
070800060906     c                   End
070900060920      * ?---------------------------------  **
071000060920      **  x DPD blank sullo status significa (1) (detto da DPD)
071100060920     c                   if        STATUS = *blank
071200060920     c                   move      '1'           STATUS
071300060920     c                   End
071400060920      **
071500060921      * ?- Aggancia il record del legame purchè non sia stato aggiornato
071600060921      * ?-  dall'altro programma con un Mancato Ritiro - SCAN 18.
071700060920     c     KDPO3         chain     fidpo03l
071800060920     c                   if        %Found(fidpo03l) and dpoSTS <> 8
071900080530     c                   clear                   DPOCODER
072000060920      **
072100060921      * ?- Controlla sull'ORM la Fase di avanzamento
072200060920     c     KOrM1         chain     fnOrm01l
072300060920      ** se addirittura non esiste il file  (Veramente Strano)
072400060920      **   imposta l'errore --> '10' x ricordare di non aver trovato l'ORM Bartolini
072500060920     c                   if        not %Found(fnorm01l)
072600060920     c                   movel     '10'          DPOCODER
072700060920     c                   else
072800060920      **
072900060921      * ?Controllo Fase ORM e/o aggiorna ORM Bartolini a seconda dello stato ricevuto
073000060920     c                   select
073100060920      **
073200060920      *?      Abbinato Parcel a Ordine DPD
073300060920     c                   when      STATUS = '1'
073400060920
073500060920     c                   if        ORmFAO <= 300
073600060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
073700060920     c                   z-add     400           ormFAO
073800060921     c                   exsr      aggior_fase
073900060921      *
074000060920     c                   else
074100061017      *
074200061017      * ?- Attenzione: occorre controllare se c'è stato un problema di ritardo dell'informazione
074300061017      * ?  di stato ricevuta in ritardo rispetto al pacco che può essere già arrivato come bolla
074400061017      * ?  import e quindi l'ORM può trovarsi già chiuso.
074500061017      * ? MA TUTTO è già stato trattato operativamente in modo corretto (NON DEVE SEGNALARE NULLA)
074600061017      * ?altrimenti
074700061017      * ? SE COSI'non fosse, occorre segnalare l'errore x essere preso in considerazione.
074800061017     c                   if        ORmFAO >=900
074900080529      *
075000080529      * comunque deve far vedere che è arrivato un legame x un Annullato
075100080529     c                   If        dpoANN <> *blank
075200080529     c                   clear                   dpoANN
075300080529     c                   end
075400061017      *
075500061017     C* aggancia l'ORM legato alla Bolla
075600061017     c                   eval      ar4trc ='M'
075700061017     c                   move      dpoPOE        dpoPOEa           3
075800061017     c                   move      dpoNRS        dpoNRSa           2
075900061017     c                   move      dpoNOR        dpoNORa           7
076000061017     c                   move      dpoNRV        dpoNRVa           2
076100070612     c****               eval      ar4n30 = %trim(dpopoeA) + %trim(dponrsA) +
076200070612     c****                                  %trim(dponorA) + %trim(dponrvA)
076300070612     c                   move      dpopoeA       §B4POE
076400070612     c                   move      dponrsA       §B4NSR
076500070612     c                   move      dponorA       §B4NOR
076600070612     c                   move      dponrvA       §B4NRV
076700070612     c                   eval      ar4n30 = dsbl4m
076800070613
076900070613     c                   move      'N'           Sped_xOrm         1
077000171019     C     Kar43         setll     fiar403l
077100171019     C     Kar43         reade     fiar403l
077200070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
077300070613     c                   dow       not %Eof(fiar403l)
077400171019     c                   movel     AR4N30        dsbl4M
077500070613     c                   if        §B4CLG = 'S'
077600070613     c                   move      'S'           Sped_xOrm         1
077700070613     c                   leave
077800070613     c                   end
077900070613     C     Kar43         reade     fiar403l
078000070613     c                   enddo
078100070613      ***
078200061017      * ?Se non trovato l'ORM legato ad una bolla invio l'errore
078300070613     c*********          if        not %Found(fiar403l)
078400070613     c                   if        Sped_xOrm = 'N'
078500061017     c                   movel     '01'          DPOCODER
078600061017     c                   else
078700061017      * ?se trovato, altro tipo di controllo sul Parcel:
078800061017      * ? La bolla generata potrebbe essere stata associata all'ORM manualmente
078900061017      * ? attribuendo UN ALTRO N.PARCEL  -->  SI DEVE SEGNALARE L'ANOMALIA.
079000171019     c******             eval      ar4trc ='I'
079100171019     C**** Kar41         chain     fiar401l
079200171019     c*****              if         %Found(fiar401l)
079300171019     c******             movel     ar4not        dsbl4i
079400171019      ***
079500171019      *  aggancia il Parcel sul nuovo file che sostituisce l'AR4 tipo "I"
079600171019     C     Kpnd          chain     fipnd01l
079700171019     c                   if         %Found(fipnd01l)
079800061017
079900061017      * ?se Parcel diverso --> segnala l'errore
080000171019     C******             if        §B4IPN  <>  parcel14
080100171019     C                   if        pndIPN  <>  parcel14
080200061017     c                   movel     '02'          DPOCODER
080300061017     c                   end
080400061017     c                   end
080500061017     c                   end
080600061017      *
080700061017     c                   else
080800061017      * ?Se non era nella fase che si aspettava imposta il codice di errore --> '01'
080900061017      * ?   sul nostro avanzamento ORM per fase non congrua.
081000080523     c***   Se sono qui l'ho già ricevuto precedentemente e non devo + registrarlo
081100080523     c*********          movel     '01'          DPOCODER
081200060920     c                   end
081300061017     c                   endIF
081400060920      **
081500060920      *?      Ritirato Parcel in DPD
081600060920     c                   when      STATUS = '2'
081700060920
081800060920     c                   if        ORmFAO = 300 or ORmFAO = 400
081900080516      **
082000080516      ** Attenzione se si tratta di un ORM di un Parcel che deve essere consegnato
082100080516      **   all'Estero il nostro ORM deve essere chiuso con fase 910 (+82 ritirato da altra filiale)
082200080516     c                   if        ormNAC <> *blank                             All'Estero
082300080516     c                   z-add     910           ormFAO
082400080516     c                   else
082500080516
082600080516      ** Se consegna in Italia:
082700060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
082800060920     c                   z-add     600           ormFAO
082900080516      **
083000080516     c                   end
083100080516      **
083200060921     c                   exsr      aggior_fase
083300060921      *
083400060920     c                   else
083500080527      *
083600080527      * ? Se NON era stato ritirato (Fase 500) ma successivamente arriva
083700080527      * ? che è stato ritirato, si devono scrivere 2 fasi:
083800080527      * ? 400 -> Assegnato (nuovamente)
083900080527      * ? 600 -> Ritirato
084000080527     c                   if        ORmFAO =500
084100080527      *   Assegna
084200080605     c                   z-add     400           new_ormFAO
084300080527     c                   exsr      aggior_fase
084400080527      *   Ritirato
084500080605     c                   clear                   new_ormFAO
084600080527     c                   z-add     600           ormFAO
084700080527     c                   exsr      aggior_fase
084800080527      *
084900080527     c                   else
085000061017      *
085100061017      * ?- Attenzione: occorre controllare se c'è stato un problema di ritardo dell'informazione
085200061017      * ?  di stato ricevuta in ritardo rispetto al pacco che può essere già arrivato come bolla
085300061017      * ?  import e quindi l'ORM può trovarsi già chiuso.
085400061017      * ? MA TUTTO è già stato trattato operativamente in modo corretto (NON DEVE SEGNALARE NULLA)
085500061017      * ?altrimenti
085600061017      * ? SE COSI'non fosse, occorre segnalare l'errore x essere preso in considerazione.
085700061017     c                   if        ORmFAO >=900
085800061017      *
085900061017     C* aggancia l'ORM legato alla Bolla
086000061017     c                   eval      ar4trc ='M'
086100061017     c                   move      dpoPOE        dpoPOEa           3
086200061017     c                   move      dpoNRS        dpoNRSa           2
086300061017     c                   move      dpoNOR        dpoNORa           7
086400061017     c                   move      dpoNRV        dpoNRVa           2
086500070613     c*****              eval      ar4n30 = %trim(dpopoeA) + %trim(dponrsA) +
086600070613     c*****                                 %trim(dponorA) + %trim(dponrvA)
086700070613     c                   move      dpopoeA       §B4POE
086800070613     c                   move      dponrsA       §B4NSR
086900070613     c                   move      dponorA       §B4NOR
087000070613     c                   move      dponrvA       §B4NRV
087100070613     c                   eval      ar4n30 = dsbl4m
087200070613     c                   move      'N'           Sped_xOrm         1
087300171019     C     Kar43         setll     fiar403l
087400171019     C     Kar43         reade     fiar403l
087500070613      *? x potenziali ORM con + colli (DPD ne gestisce solo uno x ORdine nel clearing)?
087600070613     c                   dow       not %Eof(fiar403l)
087700171019     c                   movel     AR4N30        dsbl4M
087800070613     c                   if        §B4CLG = 'S'
087900070613     c                   move      'S'           Sped_xOrm         1
088000070613     c                   leave
088100070613     c                   end
088200070613     C     Kar43         reade     fiar403l
088300070613     c                   enddo
088400070613      ***
088500061017      * ?Se non trovato l'ORM legato ad una bolla invio l'errore
088600070613     c********           if        not %Found(fiar403l)
088700070613     c                   if        Sped_xOrm = 'N'
088800061017     c                   movel     '01'          DPOCODER
088900061017     c                   else
089000061017      * ?se trovato, altro tipo di controllo sul Parcel:
089100061017      * ? La bolla generata potrebbe essere stata associata all'ORM manualmente
089200061017      * ? attribuendo UN ALTRO N.PARCEL  -->  SI DEVE SEGNALARE L'ANOMALIA.
089300171019     c*******            eval      ar4trc ='I'
089400171019     C**** Kar41         chain     fiar401l
089500171019     c*****              if         %Found(fiar401l)
089600171019     c*******            movel     ar4not        dsbl4i
089700171019      ***
089800171019      *  aggancia il Parcel sul nuovo file che sostituisce l'AR4 tipo "I"
089900171019     C     Kpnd          chain     fipnd01l
090000171019     c                   if         %Found(fipnd01l)
090100061017
090200061017      * ?se Parcel diverso --> segnala l'errore
090300171019     C******             if        §B4IPN  <>  parcel14
090400171019     C                   if        pndIPN  <>  parcel14
090500061017     c                   movel     '02'          DPOCODER
090600061017     c                   end
090700061017     c                   end
090800061017     c                   end
090900061017      *
091000061017     c                   else
091100061017      * ?Se non era nella fase che si aspettava imposta il codice di errore --> '01'
091200061017      * ?   sul nostro avanzamento ORM per fase non congrua.
091300061017     c                   movel     '01'          DPOCODER
091400061017     c                   end
091500080527      *
091600080527     c                   end
091700080527      *
091800061017     c                   endIF
091900060920      **
092000060920      *?     Stornato l'Ordine in DPD
092100060920     c                   when      STATUS = '3'
092200060920
092300060920     c                   if        ORmFAO <= 300
092400060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
092500060920     c                   z-add     999           ormFAO
092600060921     c                   exsr      aggior_fase
092700060921      *
092800060920     c                   else
092900060920      **  Se non era nella fase che si aspettava
093000060920      **   imposta il codice di errore --> '03' a ricordare lo STATUS 3 come errore
093100061214     c********           movel     '03'          DPOCODER
093200060920     c                   end
093300060920      **
093400060920      *?      Mancato Ritiro + GLI SCAN + COD.ADD.
093500060920     c                   when      STATUS = '4'
093600061120
093700080407      *   Su Chiarimento con DPD: ?
093800080516      * ?  Si è deciso di rilevare il motivo dallo Status 4 qui oppure dal STATUSDATACR
093900080516      * ?   con lo SCAN 18 e l'exception code.
094000080516      * ?  Comunque il primo che riceviamo con il motivo lo prendiamo.    ?
094100080407      *
094200061120      ** attenzione : se si tratta di un Mancato ritiro xchè presenti + parcel rispetto
094300061120      **   a quelli previsti (quindi SCAN 18 con cod.add=82) significa che occorre far fare
094400061120      **   al committente altri ORM per poter ritirare i parcel in più.
094500061120      **
094600061120      ** i dati arrivati in questo modo significa che è stato abbinato un parcel all'ORM
094700061120      **  ma ce ne sono 2 che non hanno l'ORM e che non possono essere ritirati.
094800061120      **   0163;409990;0148;01489000002045;20061109;;1;;;;;;
094900061120      **   0163;409990;0148;01489000002045;20061109;;4;;;;18;082;
095000061120      **   0163;409990;0148;01489000002045;20061109;;4;;;;18;082;
095100061120      **  NOTARE lo stesso riferimento ordine e lo stesso nr.Parcel nei records status (4)
095200080407     c*
095300080404     c                   if        zcodeX =  Parcel_inPIU
095400061120     c
095500080404     c                   movel     Parcel_inPIU  DPOCODER
095600061120     c                   add       1             DPOMANC
095700061120     c                   move      DPOSTS        STATUS
095800061120
095900061120     c                   else
096000060920
096100080527     c******             if        ORmFAO <= 400
096200080527     c                   if        ORmFAO <= 500
096300080527
096400060920      ** aggiorna avanzamento fase su testata ORM (senza distinta/padr.)
096500080516     c******             z-add     999           ormFAO
096600080516     c***  da tabella MRD:
096700080829     c                   if        manFase <> 0
096800080829
096900080516     c                   z-add     manFase       ormFAO
097000060921     c                   exsr      aggior_fase
097100080829      **
097200080829     c                   else
097300080829      **
097400080829      **  Se segnalano un Mancato ritiro NON codificato lo segnalo e non
097500080829      **   aggiorno l'ORM.
097600080829     c                   movel     '03'          DPOCODER
097700080829      **
097800080829     c                   end
097900060920      **
098000060920     c                   else
098100060920      **  Se non era nella fase che si aspettava imposta il codice di errore --> '01'
098200060920      **    sul nostro avanzamento ORM per fase non congrua.
098300060920     c                   movel     '01'          DPOCODER
098400060920     c                   end
098500060920      **
098600061120     c                   end
098700061120      **
098800060920      *?      Cancellazione non possibile in DPD
098900060920     c                   when      STATUS = '5'
099000060920
099100100723     c                   if        ORmFAO >  900
099200080513      *     rilascia record per poterlo poi riaprire
099300080513     c                   update    fnORM000
099400080513      *
099500060920      ** deve riaprire l'ORM che era stato precedentemente chiuso
099600080423      ** con Fase 100.
099700080423     c                   clear                   fior51ds
099800080423     c                   eval      I51TIP = 'A'
099900080423     c                   eval      I51POE = dpoPOE
100000080423     c                   eval      I51NSR = dpoNRS
100100080423     c                   eval      I51NOR = dpoNOR
100200080423     c                   eval      I51NRV = dpoNRV
100300080423     c                   eval      I51FAR = 100
100400080423     c                   call      'FIOR51R'
100500080423     c                   parm                    fior51ds
100600080513      **
100700080513      **  Deve riattivare il record se annullato sul FIDPO
100800080513     c                   if        O51ERR = *blank and
100900080513     c                             dpoANN <> *blank
101000080513     c                   clear                   dpoANN
101100080513     c                   end
101200060920      **
101300060921     c                   else
101400060920      **  Se non era nella fase che si aspettava imposta il codice di errore --> '01'
101500060920      **    sul nostro avanzamento ORM per fase non congrua.
101600060920     c                   movel     '01'          DPOCODER
101700060920     c                   end
101800060920      **
101900060920     c                   endsl
102000080516      *?     Finiti gli STATUS:
102100060920      **
102200060920     c                   end
102300060920      **
102400060920      **  Quindi aggiorna FIDPO con o senza l'errore con il Nr.Parcel
102500060920      **   solo se non è stato precedentemente ricevuto e aggiornato dal file
102600060920      **    degli STATI con SCAN 18
102700080516     c                   if        dpoSTS <> 8  or
102800080516     c                             dpoSTS =  8 and DPOcdmr = *blank
102900080516      **
103000060920     c                   move      *zeros        DPOSTS
103100060920     c                   move      STATUS        DPOSTS
103200080516      **
103300061002      * se mancato ritiro
103400080404     c                   if        dpoSTS =  4 and zcodeX > 0
103500080404     c                   move      zcodeX        DPOcdmr
103600061002     c                   end
103700080516      **
103800060920     c                   z-add     data_oggi     DPODTRIC
103900060920     c                   end
104000080516      **
104100080530      **  Se deve segnalare un errore riattiva il
104200080530     c                   if        DPOCODER <> *blank
104300080530     c                   MOVEL     *blank        dpoANN
104400080530     c                   end
104500080530      **
104600060920     c                   MOVEL     PKNR          DPONRP
104700060920     c                   update    fidpo000
104800170102     c
104900170102     c* Richiamo pgm per aggiornare codice bolla da franco a ass su blp se ancora "Da partire"
105000170102     c                   clear                   fieu39ds
105100170102     c                   eval      I39POE=dpopoe
105200170102     c                   eval      I39NSR=dponrs
105300170102     c                   eval      I39NOR=dponor
105400170102     c                   eval      I39NRV=dponrv
105500170102     c                   eval      I39NPR=dponrp
105600170102     c                   call      'FIEU39R'
105700170102     c                   parm                    fieu39ds
105800060920      **
105900060920     c                   end
106000060809      * ?---------------------------------  **
106100060809      **
106200060809     c                   Endsr
106300060612      * ?------------------------------------------------------------------ */
106400060921      *?      Aggiorna Fase sugli ORM
106500060612      * ?------------------------------------------------------------------ */
106600060921     c     Aggior_Fase   Begsr
106700060612      **
106800080605      * per poter gestire la scrittura delle 2 fasi consecutive
106900080605      *  e aggiornare una volta sola l'FNORM000
107000080605      **
107100080605     c                   if        new_ormFAO = 0
107200080605      **
107300170228      * imposto il campo ORMETI
107400170228     c                   eval      tbeCOD = 'FAR'
107500170228     c                   movel(p)  ormfao        tbeke1
107600170228     c     ktbe1         chain     tntbe01l
107700170228     c                   if        %Found(tntbe01l)
107800170228     c                   eval      dfar = tbeuni
107900170228     c                   eval      ormeti = d§farass
108000170228     c                   end
108100170228      ****
108200060921     c                   z-add     data_oggi     ormDFO
108300060921     c                   z-add     ore           ormOFO
108400060921     c                   update    fnORM000
108500080605      **
108600080605     c                   else
108700080605     c                   eval      ormFAO = new_ormFAO
108800080605     c                   end
108900060921
109000060921      **  Allinea le Fasi in dettaglio
109100060921     c     KOrM1         setgt     fnOrf01l
109200060921     c     KOrM1         readpe    fnOrf01l
109300060921     c                   if        not %Eof(fnorf01l)
109400080516      *
109500080516      ** Chiuso prima dell'affidamento
109600061017     c                   if           STATUS = '3'
109700061017     c                   eval         ORFCAR = '80'
109800060921     c                   end
109900080516      *
110000080516      ** Mancato Ritiro con motivo: (tabella "MRD")
110100061017     c                   if           STATUS = '4'
110200061017     c                   eval         ORFCAR = mancod
110300060921     c                   end
110400080516      **
110500080605      **  se prima era come mancato ritiro adesso riassegna e non deve
110600080605      **   impostare il codice di mancato ritiro.
110700080605     c                   if           STATUS = '2'
110800080605     c                   if        new_ormFAO = 400 and ORFFAR = 500
110900080605     c                   clear                   ORFCAR
111000080605     c                   end
111100080605     c                   end
111200080605      **
111300080516      **   Imposta Stessa Fase dell'ORM
111400060921     c                   z-add     ormFAO        ORFFAR
111500080516      **
111600080516      *  Quando Fase 910:
111700080516      *  è il caso di un ORM che deve essere ritirato e consegnato NON in ITALIA
111800080516      **  ma solo all'estero.
111900080516     c                   if        ormFAO = 910
112000080516     c                   eval      ORFCAR = Consegna_estero
112100080516     c                   end
112200080605      **
112300060921     c                   z-add     data_oggi     ORFDAE
112400060921     c                   z-add     data_oggi     ORFDFC
112500060921     c                   z-add     ore           ORFORE
112600060921     c                   eval      ORFPUE = 'ORMDPD_AUT'
112700060921     c                   write     fnorf000
112800060921     c                   end
112900060921      **
113000060921     c                   Endsr
113100060921      * ?------------------------------------------------------------------ */
113200060921      *?      Suddivide i campi della riga con carattere divisorio (;)
113300060921      * ?------------------------------------------------------------------ */
113400060921     c     Split_Rec     Begsr
113500060921      **
113600060612     c                   clear                   Nr_campi
113700060612     c                   clear                   Campi_Record
113800060612     c                   clear                   Campi_Lunghi
113900060612     c                   clear                   Campi_Numerici
114000060612     c                   clear                   Campi_Decimali
114100060612     c                   movel     ';'           Separa_campi
114200090302     c                   movel     '.'           Decimal_Separ
114300060612      *
114400060612     c                   call      'FIEU00R'
114500060612      * ?     Input
114600060612     c                   parm                    Separa_campi
114700060612     c                   parm                    Decimal_Separ
114800060612     c                   parm                    dati
114900060612      * ?     Output  su schiere di 100 elementi
115000060612     c                   parm                    Nr_campi
115100060612     c                   parm                    Campi_Record                   * i dati
115200060612     c                   parm                    Campi_Lunghi                   * lunghezza dati
115300060612     c                   parm                    Campi_Numerici                 * se numerici
115400060612     c                   parm                    Campi_Decimali                 * quanti decimali
115500060612      **
115600060612     c                   Endsr
115700971215      *----------------------------------------------------------------
115800051205      *   DEFINIZIONE CHIAVI                               *
115900060808      *----------------------------------------------------------------
116000051205     C     *INZSR        BEGSR
116100051205      *
116200991129     c     *ENTRY        PLIST
116300060613     C                   parm                    esito             1
116400060809      **
116500060809     c     Korm1         klist
116600060809     c                   kfld                    DPOPOE
116700060809     c                   kfld                    DPONRS
116800060809     c                   kfld                    DPONOR
116900060809     c                   kfld                    DPONRV
117000061017      *
117100061017     C     Kar43         KLIST
117200061017     C                   KFLD                    ar4trc
117300061017     C                   KFLD                    ar4n30
117400061017      *
117500061017     c     kar41         klist
117600061017     C                   kfld                    AR4AAS
117700061017     C                   kfld                    AR4LNP
117800061017     C                   kfld                    AR4NRS
117900061017     C                   kfld                    AR4NSP
118000061017     C                   kfld                    ar4trc
118100171019      *
118200171019     c     kpnd          klist
118300171019     C                   kfld                    AR4AAS
118400171019     C                   kfld                    AR4LNP
118500171019     C                   kfld                    AR4NRS
118600171019     C                   kfld                    AR4NSP
118700060809      **
118800061017     c     KDPO3         klist
118900060809     c                   kfld                    ODEPOT
119000060809     c                   kfld                    ORDERNR
119100170228      **
119200170228     c     Ktbe1         klist
119300170228     c                   kfld                    tbeCOD
119400170228     c                   kfld                    tbeKE1
119500060808      *
119600060808      *  imposta UDATE di elaborazione
119700060915     C                   move      *date         data_oggi
119800060915     C                   move      *date         G02inv
119900060915     C                   MOVE      *ZEROS        G02dat
120000060808     C                   MOVE      *BLANKS       G02ERR
120100060808     C                   CALL      'XSRDA8'
120200060808     C                   PARM                    WLBDAT
120300060808     C                   move      data_oggi     day
120400060808     C                   TIME                    W0140
120500060808     C                   MOVEl     W0140         ore
120600060906
120700060906      **?carica motivi mancata consegna                                     */
120800060906     c     'MRD'         setll     tntbe01l
120900060906     c                   do        *hival
121000060906     c     'MRD'         reade     tntbe01l
121100060906     c                   if        %eof(tntbe01l)
121200060906     c                   leave
121300060906     c                   endif
121400060906     c                   movel     tbeuni        dmrd
121500060906     c                   add       1             x
121600060906     c                   movel     tbeke1        manb(x)
121700060906     c                   movel     §mrdcar       mand(x)
121800060906     c                   movel     §mrdfar       manf(x)
121900060906     c                   enddo
122000060906
122100050414      *
122200991124     C                   ENDSR
122300060621      * ?------------------------------------------------------------------ */
122400060621      *?      X non bloccare in nessun caso il traduttore CLIENTI
122500060621      * ?------------------------------------------------------------------ */
122600060621     C     *pssr         BEGSR
122700060621     C
122800060710     C                   eval      esito = '2'
122900060621     C
123000060621     C                   ENDSR     '*CANCL'
123100060614      *-------------------------------------------------------------------------
