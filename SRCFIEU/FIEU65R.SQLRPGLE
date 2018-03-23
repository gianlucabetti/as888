000100150429     hDECEDIT('0,') DATEDIT(*YMD.)
000200000000      *****************************************************************
000300151106      *       Lancia tutta l'estrazione del mese di CLEARING DPD
000400000000      *****************************************************************
000500151106     Ftnecx00f  uf a E           k disk    prefix(ECX_)
000600030116      *****************************************************************
000700030116     d KPJBA         E DS
000800030120     D Tibs36ds      E DS
000900160304     D fieu64ds      e DS                  inz
001000160304     D fieu82ds      e DS                  inz
001100160304     D fieu84ds      e DS                  inz
001200151113     D*
001300000000     D CNCR80        E DS
001400000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001500151210
001600151210     d Wdata8          DS
001700151210     d  dagiolav               1      8  0
001800151210     d  agiolav                9     16  0
001900151210     d  GioLav                17     21  0
002000151210      *
002100151113      *****************************************************************
002200151113     D    wrkEsito     s              1
002300151113     D    wrkEml       s            253
002400151113     D    wrkEmlcc     s            253
002500151113     D    wrkOgg       s             44
002600151113     D    wrkMsg       s           5000
002700151113     D   MsgErr        s           5000
002800160304     D  OGGETTO        s             44
002900160304     D  MESSAGGIO      s           5000
003000151106      *****************************************************************
003100000000     D WLBDAT          DS
003200940926     D  G02DAT                 1      8  0
003300940926     D  G02INV                 9     16  0
003400940926     D  G02ERR                17     17
003500940926     D  G02TGI                18     22  0
003600151106      *
003700160308     D CMD1            S             80    DIM(1) CTDATA PERRCD(1)
003800160308      *
003900151106     D fieu88ds      e DS                  inz
004000151106     D parDate         DS
004100151106     D   DaData                1      8  0
004200151106     D    aData                9     16  0
004300151106     D    aAnno                9     12  0
004400151106     D    aMesG               13     16  0
004500151106     D   TpNtw                17     19
004600151106     D   TpData               20     20
004700151106      *
004800151106     D DATA_eur        S               D   DATFMT(*eur)
004900151106     D DATA_iso        S               D   DATFMT(*iso)
005000030205      *
005100030116     D digits          C                   CONST('0123456789')
005200030116      *****************************************************************
005300000000     C     *ENTRY        PLIST
005400000000     C                   PARM                    KPJBA
005500160304     C                   eval      Fieu64ds =  KPJBU
005600030120      *
005700030825     C* Imposta la UDATE  da TIME
005800030825     C                   TIME                    W0140            14 0
005900030825     C                   MOVE      W0140         UDATE8            8 0
006000150429     C                   MOVE      *date         Oggi              8 0
006100030825     C*
006200030120      *  In base all'utente prende il P.O.
006300030120     C                   CLEAR                   Tibs36Ds
006400030120     c                   EVAL      I36ute = Knmus
006500030120     c                   EVAL      I36Tla = 'L'
006600030120     C                   CALL      'TIBS36R'
006700030120     C                   PARM                    Tibs36Ds
006800030827     c                   movel     KNMUS         ALFA3             3
006900000000      *--------------------------------------------------------------*
007000000000      *             M A I N      L I N E
007100000000      *--------------------------------------------------------------*
007200151106     C                   SETON                                        LR
007300000000     C                   Z-ADD     1             CODUT
007400000000     C                   CALL      'X§PARUT'
007500000000     C                   PARM                    UT§DSE
007600000000     C                   MOVEL     RAGUT         RSUT             20
007700000000     C                   MOVEL     REC80         CNCR80
007800030825      *
007900160304     c                   if        e64anno >0
008000160304     c                   move      e64anno       anno
008100160304     c                   move      e64mese       mese
008200160304     c                   else
008300160304      *
008400151106      *  imposta le date di range per il calcolo della mesata precedente
008500151106     c                   move      *year         anno              4 0
008600151106     c                   move      *month        mese              2 0
008700151106      *
008800151106     c                   if        mese = 1
008900151106     c                   eval      anno = anno - 1
009000151106     c                   eval      mese = 12
009100151106     c                   else
009200151106     c                   eval      mese = mese - 1
009300151106     c                   end
009400160304      *
009500160304     c                   endIF
009600151106      *
009700151106     c                   movel     anno          annomese          6
009800151106     c                   move      mese          annomese
009900151210     C                   movel     annomese      dalAMG            8 0          DATA INIZIALE
010000151210     C                   move      '01'          dalAMG
010100151210     c     *iso          movel     DalAMG        DATA_iso
010200151210     c                   exsr      FINEmese
010300151210     C                   movel     DATA_iso      alAMG             8 0          DATA FINALE
010400151106      *
010500151110      *   Controlla la presenza delle VARIE se sono state aggiunte e le tab.
010600151110      *   del controllo fatture non sono state allineate.
010700151110     c                   clear                   totale_VAR        9 0
010800151110     C/EXEC SQL
010900151110     c+ SELECT count(*) into :TOTALE_VAR
011000151110     c+ FROM tabel00f WHERE TBLFLG = ' ' and TBLCOD ='CC' and
011100151110     c+ substr( TBLKEY, 8, 1 ) not in (SELECT substr(EVAVAR, 1, 1)
011200151110     c+    FROM tneva00f where evaann= ' ')
011300151110     C/END-EXEC
011400151110     c                   if        totale_VAR > 0
011500151110     c                   eval      messaggerr = 'Non è possibile eseguire senza-
011600151110     c                              allineare le VARIE'
011700160304     c*********          dsply                   messaggerr             99
011800151113     c                   eval      msgERR = 'Nel sistema di controllo Fatture P-
011900151113     c                             TN Estero la tabella EVA non è allineata all-
012000151113     c                             e ultime VARIE di sistema. Verificare e alli-
012100151113     c                             neare la tabella per poter proseguire con il-
012200151113     c                              caricamento del clearing'
012300160304     C                   eval      oggetto = 'Caricamento ANOMALO CLEARIN-
012400160304     c                             G - DPD FIEU65R'
012500160304     C                   eval      messaggio = %trim(Messaggerr) +':/N'+ msgERR
012600151113     c                   exsr      invia_mail
012700151110      *  ESCE forzatamente
012800151110     c                   RETURN
012900151110     c                   end
013000151110      *----------------------
013100151110      * Esegue consolidamento
013200151110     c                   clear                   fieu88ds
013300151110     c                   move      'DPD'         e88ntw
013400151110      *
013500151110     c     *iso          movel     AlAmg         DATA_iso
013600151110     c                   subdur    3:*m          DATA_iso
013700151210     c                   exsr      FINEmese
013800151110     c                   move      DATA_iso      e88dtf
013900151210      *
014000151110     c                   subdur    1:*m          DATA_iso
014100151210     c                   exsr      FINEmese
014200151110     c                   move      DATA_iso      e88dsp
014300151110      *
014400151110      *  richiama il pgm che esegue il consolidamento
014500161207     c                   if        e64sn01 ='S'
014600151110     c                   movel     fieu88ds      kpjbu
014700151110     c                   call      'FIEU89R'
014800151110     c                   parm                    kpjba
014900161207     c                   end
015000151210      *
015100151110      *----------------------
015200151106      *    CONTROLLA PRESENZA di records di DPD sul FILE DATI del PARTNER
015300151109     c                   clear                   totale_EFR        9 0
015400151106     C/EXEC SQL
015500151109     c+ SELECT count(*) into :TOTALE_EFR
015600151106     c+ FROM tnefr00f WHERE EFRNTW = 'DPD'
015700151106     c+ and EFRDFT between :dalAMG and :alAMG
015800151106     c+ and substr(EFRCAU, 1, 1) = 'N'  and efrimp>=0
015900151106     C/END-EXEC
016000151109     c                   clear                   totale_1PC        9 0
016100151109     C/EXEC SQL
016200151109     c+ SELECT count(*) into :TOTALE_1PC FROM wfdpd1pc
016300151109     C/END-EXEC
016400151109     c                   if        totale_EFR = 0
016500151110      ***
016600151109     c                   if        totale_1PC = 0
016700151106     C                   CLEAR                   MESSAGGERR       52
016800151110     c                   eval      messaggerr = 'NO dati DPD. IMPORTARE -
016900151110     c                             il Clearing DPD! su WFDPD1PC'
017000160304     c*********          dsply                   messaggerr             99
017100151113     c                   eval      msgERR = 'Manca il caricamento del file DPD -
017200151113     c                             dal Server (Intranet DICE-DPD) sul file WFDP-
017300151113     c                             D1PC. Procedere prima con il download del fi-
017400151113     c                             le dal server per poter proseguire con il ca-
017500151113     c                             ricamento del clearing'
017600160304     C                   eval      oggetto = 'Caricamento ANOMALO CLEARIN-
017700160304     c                             G - DPD FIEU65R'
017800160304     C                   eval      messaggio = %trim(Messaggerr) +':/N'+ msgERR
017900151113     c                   exsr      invia_mail
018000151109      *  ESCE forzatamente
018100151106     c                   RETURN
018200151109      *
018300151106     c                   else
018400160310     C* Prima di elaborarlo
018500160310     C*  COPIA il file ORIGINALE nel file di INPUT per mantenere i dati originali
018600161207     c                   if        e64sn02 ='S'
018700160310     c                   eval      commanl = Cmd1(1)
018800160310     c                   eval      lung = %len(%trim(commanl))
018900160310     C                   CALL      'QCMDEXC'                            99
019000160310     C                   PARM                    COMMANl         500
019100160310     C                   PARM                    LUNG             15 5
019200160310      *
019300160310      *  Importa il file originale sul WFDPDBA0F dal WFDPDBAI
019400160310     c                   call      'FIEU61RBA'
019500160310     c                   parm                    kpjba
019600161207     c                   end
019700160310     C*
019800151106      * Esegue caricamento dati DPD
019900161207     c                   if        e64sn03 ='S'
020000151106     c                   clear                   kpjbu
020100151106     c                   call      'FIEU60R'
020200151106     c                   parm                    kpjba
020300161207     c                   end
020400161207      *
020500151106     c                   end
020600151109     c                   else
020700151109     c                   if        totale_1pc > 0
020800151109     C                   CLEAR                   MESSAGGERR       52
020900151109     c                   eval      messaggerr = 'Attenzione ancora dati da scar-
021000151109     c                             icare dal WFDPD1PC!'
021100160304     c*********          dsply                   messaggerr             99
021200151113     c                   eval      msgERR = 'Non tutti i dati sono stati trasfe-
021300151113     c                             riti sul file TNEFR00F dal file WFDPD1PC. Ve-
021400151113     c                             rificare la traduzione prima di poter -
021500151113     c                             proseguire con il caricamento del clearing'
021600160304     C                   eval      oggetto = 'Caricamento ANOMALO CLEARIN-
021700160304     c                             G - DPD FIEU65R'
021800160304     C                   eval      messaggio = %trim(Messaggerr) +':/N'+ msgERR
021900151113     c                   exsr      invia_mail
022000151109      *  ESCE forzatamente
022100151109     c                   RETURN
022200151109     c                   end
022300151109      *
022400151109     c                   end
022500151110      *----------------------
022600151110      *  Controlla presenza di Causali NON codificate
022700151110     c                   clear                   totale_GRP        9 0
022800151110     C/EXEC SQL
022900151110     c+ SELECT count(*) into :TOTALE_GRP
023000151110     c+ FROM tnefr00f WHERE EFRNTW = 'DPD' and EFRDTC = 0
023100151110     c+ and EFRGRP in ('999', '   ')
023200151110     C/END-EXEC
023300151110      *
023400151110      *
023500151110     c                   if        totale_GRP = 0
023600151110     C/EXEC SQL
023700151110     c+ SELECT count(*) into :TOTALE_GRP
023800151110     c+ FROM tnecf00f WHERE ECFNTW = 'DPD' and ECFDTG = 0
023900151110     c+ and ECFGRP in ('999', '   ')
024000151110     C/END-EXEC
024100151110     c                   end
024200151110      *
024300151110     c                   if        totale_GRP > 0
024400151110     c                   eval      messaggerr = 'ATT.:Causali INDEFINITE x -
024500151110     c                             avanzare sistemare tabelle'
024600160304     c*********          dsply                   messaggerr             99
024700151113     c                   eval      msgERR = 'Attenzione sono state rilevate del-
024800151113     c                             le nuove CAUSALI ORIGINALI sul file del PTN.-
024900151113     c                             Queste devono essere inserite in tabella CFP-
025000151113     c                             . Per poter proseguire con il caricamento de-
025100151113     c                             l clearing'
025200160304     C                   eval      oggetto = 'Caricamento ANOMALO CLEARIN-
025300160304     c                             G - DPD FIEU65R'
025400160304     C                   eval      messaggio = %trim(Messaggerr) +':/N'+ msgERR
025500151113     c                   exsr      invia_mail
025600151110      *  ESCE forzatamente
025700151110     c                   RETURN
025800151110     c                   end
025900151110      **
026000151110      * Controlla e sistema i GRP = 999
026100151110      **    Sistema dati importati c/causali non associate
026200161207     c                   if        e64sn04 ='S'
026300151110     c                   call      'FIEU73R'
026400151110     c                   parm                    kpjba
026500161207     c                   end
026600151210      **
026700151109      *----------------------
026800151210      *-   PER IMPORTARE I DATI DA BRT DEVE CALCOLARE IL 2° GIORNO LAVORATIVO DI INIZIO MESE
026900151210      *-    giorno in cui viene fatto il Clearing in DPD
027000151210      *-   ED IL PRIMO GIORNO LAVORATIVO DEL MESE IN CORSO come limite prima dell'esecuzione
027100151210      *-    del Clearing.
027200151210     c     *iso          movel     DalAmg        data_iso
027300151210     c                   move      DalAmg        dagiolav
027400151210      * -----------
027500151210     c                   do        *hival
027600151210     c                   move      data_iso      agiolav
027700151210     c                   CALL      'XSRLAV8'
027800151210     c                   PARM                    Wdata8
027900151210      * se corrisponde al 2°giorno lavorativo
028000151210     c                   if        giolav = 2
028100151210     c                   z-add     agiolav       DALsecondogg      8 0
028200151210     c                   leave
028300151210     c                   else
028400151210      * altrimenti continuo a cercare il primo giorno lavorativo valido.
028500151210     c     data_iso      adddur    1:*d          data_iso
028600151210     c                   iter
028700151210     c                   endif
028800151210     c                   enddo
028900151210      * -----------
029000151210     c     *iso          movel     DalAmg        data_iso
029100151210     c                   adddur    1:*m          data_iso
029200151210     c                   move      data_iso      dagiolav
029300151210      * -----------
029400151210     c                   do        *hival
029500151210     c                   move      data_iso      agiolav
029600151210     c                   CALL      'XSRLAV8'
029700151210     c                   PARM                    Wdata8
029800151210      * se corrisponde al 1°giorno lavorativo del mese successivo ossia quello in corso
029900151210     c                   if        giolav = 1
030000151210     c                   z-add     agiolav       ALprimogg         8 0
030100151210     c                   leave
030200151210     c                   else
030300151210      * altrimenti continuo a cercare il primo giorno lavorativo valido.
030400151210     c     data_iso      adddur    1:*d          data_iso
030500151210     c                   iter
030600151210     c                   endif
030700151210     c                   enddo
030800151214      *
030900151210      *----------------------
031000151106      *    CONTROLLA PRESENZA di records di BRT sul FILE confronto PARTNER
031100151106     c                   clear                   pardate
031200151106     c                   move      'DPD'         Tpntw
031300151210     c                   z-add     DALsecondogg  DAdata
031400151210     c                   z-add     ALprimogg     Adata
031500151106      **
031600151106     c                   clear                   kpjbu
031700151106     c                   movel     pardate       kpjbu
031800151106      **
031900151106      * Esegue caricamento dati BRT
032000161207     c                   if        e64sn05 ='S'
032100151106     c                   call      'FIEU78R0'
032200151106     c                   parm                    kpjba
032300151106      *
032400151106      * Aggiorna il file che tiene conto delle estrazioni effettuate
032500151106     c     'DPD'         chain     tnecx00f
032600151210     c                   eval      ECX_data_dal = DALsecondogg
032700151210     c                   eval      ECX_data_al  = ALprimogg
032800151106     c                   eval      ECX_fatto_il  = *date
032900151106     c                   if        %Found(tnecx00f)
033000151106     c                   update    tnecx000
033100151106     c                   eLSe
033200151106     c                   eval      ECX_network = 'DPD'
033300151106     c                   write     tnecx000
033400151106     c                   end
033500161207     c                   end
033600151113      *
033700151110      *----------------------
033800151110      * Esegue INCASELLAMENTO
033900161207     c                   if        e64sn06 ='S'
034000151110     c                   clear                   kpjbu
034100151210     c                   eval       kpjbu = 'DPD' + %editc(ALprimogg:'X')
034200151110     c                   call      'FIEU79R'
034300151110     c                   parm                    kpjba
034400161207     c                   end
034500030904      *
034600151113      *----------------------
034700151113      **
034800151113      * Esegue stampa e generazione del file
034900161207     c                   if        e64sn07 ='S'
035000151113     c                   clear                   kpjbu
035100151113     c                   clear                   fieu82ds
035200151113     C                   EVAL      E82NTW = 'DPD'
035300151210     C                   EVAL      E82DTI = DalAmg
035400151210     C                   EVAL      E82DTF = AlAmg
035500151113     C                   EVAL      E82TSN = 'N'
035600151113     C                   EVAL      E82NIN = 'N'
035700151113     C                   EVAL      E82SIN = 'N'
035800151113     C                   EVAL      E82GR1 = '001'
035900151113     C                   EVAL      E82GIE = 'I'
036000151113     c                   eval       kpjbu = fieu82ds
036100151113     c                   call      'FIEU83R'
036200151113     c                   parm                    kpjba
036300161207     c                   end
036400151113      *
036500151113      *----------------------
036600151113      **
036700151113      * Esegue stampa e generazione del file
036800161207     c                   if        e64sn08 ='S'
036900151113     c                   clear                   kpjbu
037000151113     c                   clear                   fieu84ds
037100151113     C                   EVAL      E84NTW = 'DPD'
037200151210     C                   EVAL      E84DIF = DALsecondogg
037300151210     C                   EVAL      E84DFF = ALprimogg
037400151113     C                   EVAL      E84IF  = 'S'
037500151113     C                   EVAL      E84IFR = 'X'
037600151113     C                   EVAL      E84IA  = 'S'
037700151113     C                   EVAL      E84IAC = 'X'
037800151113     C                   EVAL      E84EF  = 'S'
037900151113     C                   EVAL      E84EFC = 'X'
038000151113     C                   EVAL      E84EA  = 'S'
038100151113     C                   EVAL      E84EAR = 'X'
038200151113     C                   EVAL      E84DET = 'N'
038300151113     C                   EVAL      E84TOF = 'S'
038400151113     C                   EVAL      E84TOR = 'N'
038500151113     C                   EVAL      E84TOS = 'N'
038600151113     C                   EVAL      E84TOP = 'N'
038700151113     C                   EVAL      E84SNA = 'S'
038800151113     C                   EVAL      E84SNI = 'S'
038900151113     C                   EVAL      E84SDE = 099
039000151211     C                   EVAL      E84FPC = 'N'
039100151113     C                   EVAL      E84GR1 = '001'
039200151113     C                   EVAL      E84GIE = 'I'
039300151113     C                   EVAL      E84RIE = 'S'
039400151113     c                   eval       kpjbu = fieu84ds
039500160308      *  al momento non glielo facciamo fare
039600160308      **
039700160308     c                   goto      after_Fieu85R
039800151113     c                   call      'FIEU85R'
039900151113     c                   parm                    kpjba
040000160308     c     after_Fieu85R tag
040100160304      *
040200160308      *
040300160304     C                   eval       OGGETTO = 'Importazione CLEARING OK'
040400160512      *
040500160512     C                   eval       MESSAGGIO = 'Il CLEARING con data Fattura: '
040600160512     c                             + %editw(ALamg:'    /  /  ')
040700160512     C                   eval       MESSAGGIO = %trim(MESSAGGIO) +
040800160512     c                             ' è stato caricato correttamente sul -
040900160512     c                             sistema. Si può procedere con -
041000160512     c                             con il Confronto Dati Partner con Dati BRT.'
041100160304     c                   exsr      invia_mail
041200161207     c                   end
041300160930      **
041400160930      *----------------------
041500160930      **
041600161012      *  per FUORI MISURA e ISOLE (consegne estero 0844)
041700160930      **   passa il range di date delimitanti il mese in esame
041800161207     c                   if        e64sn09 ='S'
041900160930     c                   clear                   kpjbu
042000160930     c                   eval      kpjbu= %editc(dalAMG:'X') + %editc(alAMG:'X')
042100161012     c                   call      'FIEU65R1C'
042200161012     c                   parm                    kpjba
042300161207     c                   end
042400161012      **
042500161012      *  per i DIROTTAMENTI       (consegne estero 0844) genera Bolle Recupero
042600161012      **   passa il range di date delimitanti il mese in esame
042700161207     c                   if        e64sn10 ='S'
042800161012     c                   clear                   kpjbu
042900161012     c                   eval      kpjbu= %editc(dalAMG:'X') + %editc(alAMG:'X')
043000161012     c                   call      'FIEU65R2C'
043100161012     c                   parm                    kpjba
043200161012     c                   end
043300030116      *
043400000000     C     FINE          TAG
043500151106     c                   RETURN
043600000000      *--------------------------------------------------------------*
043700151113      *   INVIA MAIL
043800151113      *____________________________________________________________________
043900151113     c     INVIA_MAIL    BEGSR
044000151113      *
044100151113     C* Inizializzo variabili
044200160610     C                   eval      wrkEml = 'Stefano.Solfrini@BRT.it'
044300151113     C                   eval      wrkEmlcc = 'Andrea.Bertocchi@brt.it'
044400160527     C                   eval      wrkOgg = OGGETTO
044500160304     C                   eval      wrkMsg = MESSAGGIO
044600151113      *
044700151113      * Sostituito perchè il TIS701C ha chiodato anche l'indirizzo CED
044800151113      *  da non modificare:
044900151113     C                   call(e)   'TIS701C1'
045000151113     C                   parm                    wrkEml
045100151113     C                   parm                    wrkEmlcc
045200151113     C                   parm                    wrkOgg
045300151113     C                   parm                    wrkMsg
045400151113     C                   parm                    wrkesito
045500151113      *
045600151113     c                   ENDSR
045700151113      * ?------------------------------------------------------------------ */
045800151210      *
045900151210      * ?------------------------------------------------------------------ */
046000151210     c     FINEMESE      BEGSR
046100151210      *
046200151210     C                   MOVEL     DATA_ISO      data08            8 0
046300151210     C                   MOVEL     Data08        aaaa04            4 0
046400151210     C                   move      '31'          data08
046500151210     C                   move      data08        mmgg04            4 0
046600151210     C                   movel     mmgg04        mm02              2 0
046700151210     c                   if        mm02=4 or mm02=6 or mm02=9 or mm02=11
046800151210     C                   move      '30'          data08
046900151210     c                   end
047000151210     c     aaaa04        div       4             risulta           3 0
047100151210     c                   mvr                     resto             3 0
047200151210     c                   if        mm02=2
047300151210     C                   move      '28'          data08
047400151210     c                   if        resto = 0
047500151210     C                   move      '29'          data08
047600151210     c                   end
047700151210     c                   end
047800151210     C     *iso          MOVEL     data08        DATA_ISO
047900151210      *
048000151210     c                   ENDSR
048100151210      * ?------------------------------------------------------------------ */
048200160308**         CMD1
048300160310CPYF FROMFILE(WFDPD1PC) TOFILE(WFDPDBAI) MBROPT(*REPLACE) FMTOPT(*NOCHK)
