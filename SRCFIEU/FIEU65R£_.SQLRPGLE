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
014500151110     c                   movel     fieu88ds      kpjbu
014600151110     c                   call      'FIEU89R'
014700151110     c                   parm                    kpjba
014800151210      *
014900151110      *----------------------
015000151106      *    CONTROLLA PRESENZA di records di DPD sul FILE DATI del PARTNER
015100151109     c                   clear                   totale_EFR        9 0
015200151106     C/EXEC SQL
015300151109     c+ SELECT count(*) into :TOTALE_EFR
015400151106     c+ FROM tnefr00f WHERE EFRNTW = 'DPD'
015500151106     c+ and EFRDFT between :dalAMG and :alAMG
015600151106     c+ and substr(EFRCAU, 1, 1) = 'N'  and efrimp>=0
015700151106     C/END-EXEC
015800151109     c                   clear                   totale_1PC        9 0
015900151109     C/EXEC SQL
016000151109     c+ SELECT count(*) into :TOTALE_1PC FROM wfdpd1pc
016100151109     C/END-EXEC
016200151109     c                   if        totale_EFR = 0
016300151110      ***
016400151109     c                   if        totale_1PC = 0
016500151106     C                   CLEAR                   MESSAGGERR       52
016600151110     c                   eval      messaggerr = 'NO dati DPD. IMPORTARE -
016700151110     c                             il Clearing DPD! su WFDPD1PC'
016800160304     c*********          dsply                   messaggerr             99
016900151113     c                   eval      msgERR = 'Manca il caricamento del file DPD -
017000151113     c                             dal Server (Intranet DICE-DPD) sul file WFDP-
017100151113     c                             D1PC. Procedere prima con il download del fi-
017200151113     c                             le dal server per poter proseguire con il ca-
017300151113     c                             ricamento del clearing'
017400160304     C                   eval      oggetto = 'Caricamento ANOMALO CLEARIN-
017500160304     c                             G - DPD FIEU65R'
017600160304     C                   eval      messaggio = %trim(Messaggerr) +':/N'+ msgERR
017700151113     c                   exsr      invia_mail
017800151109      *  ESCE forzatamente
017900151106     c                   RETURN
018000151109      *
018100151106     c                   else
018200160310     C* Prima di elaborarlo
018300160310     C*  COPIA il file ORIGINALE nel file di INPUT per mantenere i dati originali
018400160310     c                   eval      commanl = Cmd1(1)
018500160310     c                   eval      lung = %len(%trim(commanl))
018600160310     C                   CALL      'QCMDEXC'                            99
018700160310     C                   PARM                    COMMANl         500
018800160310     C                   PARM                    LUNG             15 5
018900160310      *
019000160310      *  Importa il file originale sul WFDPDBA0F dal WFDPDBAI
019100160310     c                   call      'FIEU61RBA'
019200160310     c                   parm                    kpjba
019300160310      *
019400160310     C*
019500151106      * Esegue caricamento dati DPD
019600151106     c                   clear                   kpjbu
019700151106     c                   call      'FIEU60R'
019800151106     c                   parm                    kpjba
019900151106     c                   end
020000151109     c                   else
020100151109     c                   if        totale_1pc > 0
020200151109     C                   CLEAR                   MESSAGGERR       52
020300151109     c                   eval      messaggerr = 'Attenzione ancora dati da scar-
020400151109     c                             icare dal WFDPD1PC!'
020500160304     c*********          dsply                   messaggerr             99
020600151113     c                   eval      msgERR = 'Non tutti i dati sono stati trasfe-
020700151113     c                             riti sul file TNEFR00F dal file WFDPD1PC. Ve-
020800151113     c                             rificare la traduzione prima di poter -
020900151113     c                             proseguire con il caricamento del clearing'
021000160304     C                   eval      oggetto = 'Caricamento ANOMALO CLEARIN-
021100160304     c                             G - DPD FIEU65R'
021200160304     C                   eval      messaggio = %trim(Messaggerr) +':/N'+ msgERR
021300151113     c                   exsr      invia_mail
021400151109      *  ESCE forzatamente
021500151109     c                   RETURN
021600151109     c                   end
021700151109      *
021800151109     c                   end
021900151110      *----------------------
022000151110      *  Controlla presenza di Causali NON codificate
022100151110     c                   clear                   totale_GRP        9 0
022200151110     C/EXEC SQL
022300151110     c+ SELECT count(*) into :TOTALE_GRP
022400151110     c+ FROM tnefr00f WHERE EFRNTW = 'DPD' and EFRDTC = 0
022500151110     c+ and EFRGRP in ('999', '   ')
022600151110     C/END-EXEC
022700151110      *
022800151110      *
022900151110     c                   if        totale_GRP = 0
023000151110     C/EXEC SQL
023100151110     c+ SELECT count(*) into :TOTALE_GRP
023200151110     c+ FROM tnecf00f WHERE ECFNTW = 'DPD' and ECFDTG = 0
023300151110     c+ and ECFGRP in ('999', '   ')
023400151110     C/END-EXEC
023500151110     c                   end
023600151110      *
023700151110     c                   if        totale_GRP > 0
023800151110     c                   eval      messaggerr = 'ATT.:Causali INDEFINITE x -
023900151110     c                             avanzare sistemare tabelle'
024000160304     c*********          dsply                   messaggerr             99
024100151113     c                   eval      msgERR = 'Attenzione sono state rilevate del-
024200151113     c                             le nuove CAUSALI ORIGINALI sul file del PTN.-
024300151113     c                             Queste devono essere inserite in tabella CFP-
024400151113     c                             . Per poter proseguire con il caricamento de-
024500151113     c                             l clearing'
024600160304     C                   eval      oggetto = 'Caricamento ANOMALO CLEARIN-
024700160304     c                             G - DPD FIEU65R'
024800160304     C                   eval      messaggio = %trim(Messaggerr) +':/N'+ msgERR
024900151113     c                   exsr      invia_mail
025000151110      *  ESCE forzatamente
025100151110     c                   RETURN
025200151110     c                   end
025300151110      **
025400151110      * Controlla e sistema i GRP = 999
025500151110      **    Sistema dati importati c/causali non associate
025600151110     c                   call      'FIEU73R'
025700151110     c                   parm                    kpjba
025800151210      **
025900151109      *----------------------
026000151210      *-   PER IMPORTARE I DATI DA BRT DEVE CALCOLARE IL 2° GIORNO LAVORATIVO DI INIZIO MESE
026100151210      *-    giorno in cui viene fatto il Clearing in DPD
026200151210      *-   ED IL PRIMO GIORNO LAVORATIVO DEL MESE IN CORSO come limite prima dell'esecuzione
026300151210      *-    del Clearing.
026400151210     c     *iso          movel     DalAmg        data_iso
026500151210     c                   move      DalAmg        dagiolav
026600151210      * -----------
026700151210     c                   do        *hival
026800151210     c                   move      data_iso      agiolav
026900151210     c                   CALL      'XSRLAV8'
027000151210     c                   PARM                    Wdata8
027100151210      * se corrisponde al 2°giorno lavorativo
027200151210     c                   if        giolav = 2
027300151210     c                   z-add     agiolav       DALsecondogg      8 0
027400151210     c                   leave
027500151210     c                   else
027600151210      * altrimenti continuo a cercare il primo giorno lavorativo valido.
027700151210     c     data_iso      adddur    1:*d          data_iso
027800151210     c                   iter
027900151210     c                   endif
028000151210     c                   enddo
028100151210      * -----------
028200151210     c     *iso          movel     DalAmg        data_iso
028300151210     c                   adddur    1:*m          data_iso
028400151210     c                   move      data_iso      dagiolav
028500151210      * -----------
028600151210     c                   do        *hival
028700151210     c                   move      data_iso      agiolav
028800151210     c                   CALL      'XSRLAV8'
028900151210     c                   PARM                    Wdata8
029000151210      * se corrisponde al 1°giorno lavorativo del mese successivo ossia quello in corso
029100151210     c                   if        giolav = 1
029200151210     c                   z-add     agiolav       ALprimogg         8 0
029300151210     c                   leave
029400151210     c                   else
029500151210      * altrimenti continuo a cercare il primo giorno lavorativo valido.
029600151210     c     data_iso      adddur    1:*d          data_iso
029700151210     c                   iter
029800151210     c                   endif
029900151210     c                   enddo
030000151214      *
030100151210      *----------------------
030200151106      *    CONTROLLA PRESENZA di records di BRT sul FILE confronto PARTNER
030300151106     c                   clear                   pardate
030400151106     c                   move      'DPD'         Tpntw
030500151210     c                   z-add     DALsecondogg  DAdata
030600151210     c                   z-add     ALprimogg     Adata
030700151106      **
030800151106     c                   clear                   kpjbu
030900151106     c                   movel     pardate       kpjbu
031000151106      **
031100151106      * Esegue caricamento dati BRT
031200151106     c                   call      'FIEU78R0'
031300151106     c                   parm                    kpjba
031400151106      *
031500151106      * Aggiorna il file che tiene conto delle estrazioni effettuate
031600151106     c     'DPD'         chain     tnecx00f
031700151210     c                   eval      ECX_data_dal = DALsecondogg
031800151210     c                   eval      ECX_data_al  = ALprimogg
031900151106     c                   eval      ECX_fatto_il  = *date
032000151106     c                   if        %Found(tnecx00f)
032100151106     c                   update    tnecx000
032200151106     c                   eLSe
032300151106     c                   eval      ECX_network = 'DPD'
032400151106     c                   write     tnecx000
032500151106     c                   end
032600151113      *
032700151110      *----------------------
032800151110      **
032900151110      * Esegue INCASELLAMENTO
033000151110     c                   clear                   kpjbu
033100151210     c                   eval       kpjbu = 'DPD' + %editc(ALprimogg:'X')
033200151110     c                   call      'FIEU79R'
033300151110     c                   parm                    kpjba
033400030904      *
033500151113      *----------------------
033600151113      **
033700151113      * Esegue stampa e generazione del file
033800151113     c                   clear                   kpjbu
033900151113     c                   clear                   fieu82ds
034000151113     C                   EVAL      E82NTW = 'DPD'
034100151210     C                   EVAL      E82DTI = DalAmg
034200151210     C                   EVAL      E82DTF = AlAmg
034300151113     C                   EVAL      E82TSN = 'N'
034400151113     C                   EVAL      E82NIN = 'N'
034500151113     C                   EVAL      E82SIN = 'N'
034600151113     C                   EVAL      E82GR1 = '001'
034700151113     C                   EVAL      E82GIE = 'I'
034800151113     c                   eval       kpjbu = fieu82ds
034900151113     c                   call      'FIEU83R'
035000151113     c                   parm                    kpjba
035100151113      *
035200151113      *----------------------
035300151113      **
035400151113      * Esegue stampa e generazione del file
035500151113     c                   clear                   kpjbu
035600151113     c                   clear                   fieu84ds
035700151113     C                   EVAL      E84NTW = 'DPD'
035800151210     C                   EVAL      E84DIF = DALsecondogg
035900151210     C                   EVAL      E84DFF = ALprimogg
036000151113     C                   EVAL      E84IF  = 'S'
036100151113     C                   EVAL      E84IFR = 'X'
036200151113     C                   EVAL      E84IA  = 'S'
036300151113     C                   EVAL      E84IAC = 'X'
036400151113     C                   EVAL      E84EF  = 'S'
036500151113     C                   EVAL      E84EFC = 'X'
036600151113     C                   EVAL      E84EA  = 'S'
036700151113     C                   EVAL      E84EAR = 'X'
036800151113     C                   EVAL      E84DET = 'N'
036900151113     C                   EVAL      E84TOF = 'S'
037000151113     C                   EVAL      E84TOR = 'N'
037100151113     C                   EVAL      E84TOS = 'N'
037200151113     C                   EVAL      E84TOP = 'N'
037300151113     C                   EVAL      E84SNA = 'S'
037400151113     C                   EVAL      E84SNI = 'S'
037500151113     C                   EVAL      E84SDE = 099
037600151211     C                   EVAL      E84FPC = 'N'
037700151113     C                   EVAL      E84GR1 = '001'
037800151113     C                   EVAL      E84GIE = 'I'
037900151113     C                   EVAL      E84RIE = 'S'
038000151113     c                   eval       kpjbu = fieu84ds
038100160308      *  al momento non glielo facciamo fare
038200160308      **
038300160308     c                   goto      after_Fieu85R
038400151113     c                   call      'FIEU85R'
038500151113     c                   parm                    kpjba
038600160308     c     after_Fieu85R tag
038700160304      *
038800160308      *
038900160304     C                   eval       OGGETTO = 'Importazione CLEARING OK'
039000160512      *
039100160512     C                   eval       MESSAGGIO = 'Il CLEARING con data Fattura: '
039200160512     c                             + %editw(ALamg:'    /  /  ')
039300160512     C                   eval       MESSAGGIO = %trim(MESSAGGIO) +
039400160512     c                             ' è stato caricato correttamente sul -
039500160512     c                             sistema. Si può procedere con -
039600160512     c                             con il Confronto Dati Partner con Dati BRT.'
039700160304     c                   exsr      invia_mail
039800160930      **
039900160930      *----------------------
040000160930      **
040100161012      *  per FUORI MISURA e ISOLE (consegne estero 0844)
040200160930      **   passa il range di date delimitanti il mese in esame
040300160930     c                   if        e64brec ='S'
040400160930     c                   clear                   kpjbu
040500160930     c                   eval      kpjbu= %editc(dalAMG:'X') + %editc(alAMG:'X')
040600161012     c                   call      'FIEU65R1C'
040700161012     c                   parm                    kpjba
040800161012      **
040900161012      *  per i DIROTTAMENTI       (consegne estero 0844) genera Bolle Recupero
041000161012      **   passa il range di date delimitanti il mese in esame
041100161012     c                   clear                   kpjbu
041200161012     c                   eval      kpjbu= %editc(dalAMG:'X') + %editc(alAMG:'X')
041300161012     c                   call      'FIEU65R2C'
041400161012     c                   parm                    kpjba
041500161012     c                   end
041600030116      *
041700000000     C     FINE          TAG
041800151106     c                   RETURN
041900000000      *--------------------------------------------------------------*
042000151113      *   INVIA MAIL
042100151113      *____________________________________________________________________
042200151113     c     INVIA_MAIL    BEGSR
042300151113      *
042400151113     C* Inizializzo variabili
042500160610     C                   eval      wrkEml = 'Stefano.Solfrini@BRT.it'
042600151113     C                   eval      wrkEmlcc = 'Andrea.Bertocchi@brt.it'
042700160527     C                   eval      wrkOgg = OGGETTO
042800160304     C                   eval      wrkMsg = MESSAGGIO
042900151113      *
043000151113      * Sostituito perchè il TIS701C ha chiodato anche l'indirizzo CED
043100151113      *  da non modificare:
043200151113     C                   call(e)   'TIS701C1'
043300151113     C                   parm                    wrkEml
043400151113     C                   parm                    wrkEmlcc
043500151113     C                   parm                    wrkOgg
043600151113     C                   parm                    wrkMsg
043700151113     C                   parm                    wrkesito
043800151113      *
043900151113     c                   ENDSR
044000151113      * ?------------------------------------------------------------------ */
044100151210      *
044200151210      * ?------------------------------------------------------------------ */
044300151210     c     FINEMESE      BEGSR
044400151210      *
044500151210     C                   MOVEL     DATA_ISO      data08            8 0
044600151210     C                   MOVEL     Data08        aaaa04            4 0
044700151210     C                   move      '31'          data08
044800151210     C                   move      data08        mmgg04            4 0
044900151210     C                   movel     mmgg04        mm02              2 0
045000151210     c                   if        mm02=4 or mm02=6 or mm02=9 or mm02=11
045100151210     C                   move      '30'          data08
045200151210     c                   end
045300151210     c     aaaa04        div       4             risulta           3 0
045400151210     c                   mvr                     resto             3 0
045500151210     c                   if        mm02=2
045600151210     C                   move      '28'          data08
045700151210     c                   if        resto = 0
045800151210     C                   move      '29'          data08
045900151210     c                   end
046000151210     c                   end
046100151210     C     *iso          MOVEL     data08        DATA_ISO
046200151210      *
046300151210     c                   ENDSR
046400151210      * ?------------------------------------------------------------------ */
046500160308**         CMD1
046600160310CPYF FROMFILE(WFDPD1PC) TOFILE(WFDPDBAI) MBROPT(*REPLACE) FMTOPT(*NOCHK)
