000100131003     H DECEDIT('0.') DATEDIT(*YMD.)
000200131003     H* TRTCT81AR ---------------------------------------------------*
000300131007     H* ?__ ATTENZIONE l'UNB di AMAZON è CHIODATO x fare prima _____ *
000400131007     H* ?__  soprattutto x la fase di TEST. ________________________ *
000500900418     H*                                                              *
000600131003     H* Scrittura del file nel formato AMAZON IFTSTA particolare     *
000700120830     H*                                                              *
000800120830     H*  Utilizza il EDIFTSTA in QTEMP come Workfile generico        *
000900131003     H*                                                              *
001000131003     H* ?_ GLI stati AMAZON sono particolari  tradotti su EDSTS ____ *
001100131003     H* ?__ e hanno solo il secondo CODICE  --> MAI il primo _______ *
001200131003     H*                                                              *
001300120830     H*--------------------------------------------------------------*
001400121022     FEDIFTSTA  uF A E             DISK    usropn
001500120830     FEDsts02L  iF   E           K DISK
001600131008     FfnARB01L  iF   E           K DISK
001700131008     FazORG01L  iF   E           K DISK
001800120830      **?__________________________________________________________________ */
001900120830     ftivgd00f  if a E             DISK    usropn commit
002000110617     Ftivir02L  IF   E           K DISK
002100110617     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
002200110617     F                                     PREFIX(f_) usropn
002300110617      **?__________________________________________________________________ */
002400120830     D Var_ISV         s                   like(Virfi1)
002500110617     D TipoF           C                   CONST('EA')
002600110617     D dwlprg          s             10    INZ(*all'0')
002700110617     D wrkprg          s              8  0 INZ(*zeros)
002800060814     d trul47ds      e ds
002900120830      *
003000120830     D DataOra         DS            12
003100120830     D Data                           8
003200120830     D Ora                            4
003300120830      *
003400131003     D bolla14         DS            14
003500131003     D anno2                          2s 0
003600131003     D linea                          3s 0
003700131003     D serie                          2s 0
003800131003     D numspe                         7s 0
003900131003      *
004000131003     d Solo_prima_volta...
004100131003     d                 s              1
004200120830     d  Firma          s             20
004300110316     D Segmento        s           2048
004400110316     D Conta_righe     s              5i 0
004500131003     D Conta_segm      s              5i 0
004600131007     D Conta_UNH       s              5s 0
004700110621     d  esito          s              1
004800131007     d solo_una_volta  s              1    inz('S')
004900120830      **?__________________________________________________________________ */
005000110316     D* DS
005100120830      **?__________________________________________________________________ */
005200110316     D* .. per scompozione dati da spedire a seconda del tipo record
005300131003     D ST00ds        E DS                  EXTNAME(EDST00DS)
005400131003     D ST05ds        E DS                  EXTNAME(EDST05DS)
005500131003     D ST10ds        E DS                  EXTNAME(EDST10DS)
005600131003     D  T10                   48    103
005700131003     D                                     DIM(2)
005800131003     D ST15ds        E DS                  EXTNAME(EDST15DS)
005900131003     D  T15                   13    362
006000131003     D                                     DIM(5)
006100131003      *
006200120830     D SD00ds        E DS                  EXTNAME(EDSD00DS)
006300120830     D SD05ds        E DS                  EXTNAME(EDSD05DS)
006400110316     D SD10ds        E DS                  EXTNAME(EDSD10DS)
006500110316     D SD15ds        E DS                  EXTNAME(EDSD15DS)
006600110316     D  D15                   13    362
006700110316     D                                     DIM(5)
006800110316     D SD20ds        E DS                  EXTNAME(EDSD20DS)
006900110316     D  D20blk                10     22
007000110316     D  D20                   33    732
007100110316     D                                     DIM(20)
007200120830      **?__________________________________________________________________ */
007300131007     D UNB_sgm         C                   CONST('UNB+UNOA:1+BRTXX-IT:14+545053-
007400131007     d                                     4000000:14')
007500131007     D UNH_sgm1        C                   CONST('UNH+')
007600131007     D UNH_sgm2        C                   CONST('+IFTSTA:D:01A:UN:EAN008')
007700131003     D BGM_sgm         C                   CONST('BGM+23+')
007800131003     D NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
007900131003     D NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
008000131003     D NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
008100131003     D NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
008200131003     D NAD_SF_5        C                   CONST('NNI++29015+IT')
008300131003     D NAD_ST          C                   CONST('NAD+ST++')
008400131008     D LOC_sgm1        C                   CONST('LOC+175+')
008500131008     D LOC_sgm2        C                   CONST('::3:')
008600131008     D LOC_sgm3        C                   CONST('+IT')
008700131003     D CNI_sgm         C                   CONST('CNI+1')
008800131003     D STS_sgm         C                   CONST('STS+1++')
008900131003     D RFF_sgm         C                   CONST('RFF+POR:')
009000131003     D TDT_sgm         C                   CONST('TDT+30+1+++BRTXX')
009100131003     D DTM_sgm         C                   CONST('DTM+ZZZ:')
009200131003     D EQD_sgm         C                   CONST('EQD+AA')
009300131003     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
009400131003     D GID_sgm         C                   CONST('GID+1+1:CTN')
009500131003     D UNT_sgm         C                   CONST('UNT+')
009600131003     D UNZ_sgm         C                   CONST('UNZ+1+')
009700131003     D piu             C                   CONST('+')
009800131003     D diviso          C                   CONST(':')
009900131003     D apice           C                   CONST('''')
010000131003     D digits          C                   CONST('1234567890')
010100120830      **?__________________________________________________________________ */
010200110405     I* FILE
010300120830      **?__________________________________________________________________ */
010400110405     IIFTSTA00
010500110405     I              SSIFTSTA                    staDAT
010600050927      **?__________________________________________________________________ */
010700050704      *    main
010800110316      **?__________________________________________________________________ */
010900110316     c     *entry        plist                                                   esempio
011000131003     C                   PARM                    CLIENTE           8            "01662139"
011100110316     C                   PARM                    Tipo_DS           4            "ST00"
011200110316     C                   PARM                    DsGenerica     1024             EDST00DS
011300110601     C                   PARM                    Numero_Righe      5 0          nnnnn
011400110617     C                   PARM                    Progressivo      10
011500110621     c                   PARM                    esito             1
011600110617     c*
011700110617     c     kvir02        klist
011800110617     C                   kfld                    virKSC
011900110617     C                   kfld                    virTIP
012000120830     c*
012100131003     c     karb01        klist
012200131003     C                   kfld                    arbAAS
012300131003     C                   kfld                    arbLNP
012400131003     C                   kfld                    arbNRS
012500131003     C                   kfld                    arbNSP
012600131003     c*
012700120830     c     kedSTS        klist
012800120830     C                   kfld                    STSUNBCLI
012900120830     C                   kfld                    STSCODSTSB
013000120830     C                   kfld                    STSSUBSTSB
013100110322     c*
013200110322     C                   z-add     Numero_Righe  Conta_righe
013300110316     c*
013400110316     C                   TIME                    ORADAT           14 0
013500110316     c                   move      *date         udtymd            8 0
013600120830     C                   TIME                    W0140            14 0
013700110316     C                   MOVE      W0140         W0080             8 0
013800120830     c                   move      W0140         dati_Oramin      14
013900131003     c                   movel     W0140         Oramin            6 0
014000131003     c                   movel     Oramin        Oraminuti         4 0
014100131003     c                   move      W0140         Data8             8 0
014200131003     c                   move      Data8         Data6             6 0
014300131003     c                   movel     Data8         Data4             4 0
014400131003     c                   movel     Data4         Data6
014500120830
014600120830      * salva la testat in QTEMP
014700120830     c                   open      EDIFTSTA                             99
014800050927      **?__________________________________________________________________ */
014900131003      * Decodifica le DS
015000110316      **?__________________________________________________________________ */
015100131007     c                   eval        TAPRG = DsGenerica
015200131007      *
015300110316     c                   select
015400131003      *
015500131003     c                   when      Tipo_DS = 'ST00'
015600131003     c                   eval        ST00ds = DsGenerica
015700131003      *
015800131003     c                   when      Tipo_DS = 'ST05'
015900131003     c                   eval        ST05ds = DsGenerica
016000131003      *
016100131003     c                   when      Tipo_DS = 'ST10'
016200131003     c                   eval        ST10ds = DsGenerica
016300131003      *
016400131003     c                   when      Tipo_DS = 'ST15'
016500131003     c                   eval        ST15ds = DsGenerica
016600110316
016700110405      * da qui inizia il dettaglio
016800131003      *   CNI
016900110405     c                   when      Tipo_DS = 'SD00'
017000110316     c                   eval        SD00ds = DsGenerica
017100120830      *   STS RFF DTM
017200110316     c                   when      Tipo_DS = 'SD05'
017300110316     c                   eval        SD05ds = DsGenerica
017400120830      *   NAD
017500110316     c                   when      Tipo_DS = 'SD10'
017600110316     c                   eval        SD10ds = DsGenerica
017700120830      *   FTX
017800110316     c                   when      Tipo_DS = 'SD15'
017900110316     c                   eval        SD15ds = DsGenerica
018000120830      *   GID PCI
018100110316     c                   when      Tipo_DS = 'SD20'
018200110316     c                   eval        SD20ds = DsGenerica
018300131003      *
018400120830      **   scrive max.20 righe quanti i codici a barre contenuti
018500120830     c                   do        20            X                 3 0
018600120830     c                   if        D20(x)  = *blank
018700120830     c                   leave
018800120830     c                   end
018900120830     c                   movel     D20(x)        BARcode          35
019000120830     c                   eval      campo_alfa  = BARcode
019100120830     c                   exsr      Char_Speciali
019200120830     c                   eval      BARcode     = campo_alfa
019300120830     c                   enddo
019400131003      *
019500131003     c                   exsr      WRI_IFTSTA
019600131003      *
019700110316     c                   when      Tipo_DS = 'FINE'
019800131007     c                   exsr      WRI_FINE
019900131007      *
020000131007     C                   z-add     Conta_righe   Numero_Righe
020100131007     c                   exsr      SCARICA_QTEMP
020200131007      *
020300121022     c                   seton                                        LR
020400121022     c                   RETURN
020500110405
020600110405      * se servisse gestire qualcosa come un "ROLLBACK" FITTIZIO
020700110405      *  si deve impostare in questo punto del programma.
020800110405     c                   when      Tipo_DS = 'RLBK'
020900110405     c                   exsr      ROLL_BCK
021000121022      *
021100121022     C                   z-add     Conta_righe   Numero_Righe
021200121022     c                   seton                                        LR
021300121022     c                   RETURN
021400110316
021500110316     C                   ENDSL
021600050707      *
021700110322      *  per poter contare in totale le righe deve ripassare a che punto è arrivato
021800110322     C                   z-add     Conta_righe   Numero_Righe
021900110322      *
022000121022     c                   seton                                        RT
022100121022     c                   RETURN
022200120830      **?__________________________________________________________________ */
022300131003      **    Scrittura  righe da QTEMP dal EDIFTSTA
022400120830      **?__________________________________________________________________ */
022500131003     C     SCARICA_QTEMP Begsr
022600131003      *
022700131003      * dal primo record scritto  CHIUDE E RIAPRE in Qtemp
022800131003     c                   close     EDIFTSTA                             99
022900131003      * Apre
023000131003      *   il TIVGD
023100131003     c                   open      TIVGD00F                             99
023200131003     C                   EXSR      vgd_OPEN
023300131003      * Lo scrive
023400131003     C                   EXSR      copia_in_VGD
023500131003      * quindi lo chiude
023600131003     C                   EXSR      vgd_CLOSE
023700131003     c                   close     TIVGD00F                             99
023800131003
023900131003      * conferma la scrittura del TIVGD
024000131003     c                   commit
024100131003      *
024200131003     c                   endsr
024300131003      **?__________________________________________________________________ */
024400131003      **    Scrittura  righe di testata in QTEMP nel EDIFTSTA
024500131003      **?__________________________________________________________________ */
024600131003     C     WRI_IFTSTA    Begsr
024700131003      *
024800131003      *   Compone le righe formato AMAZON
024900131003      *----
025000131024      *  Inizio DETTAGLIO
025100131003     c                   exsr      DECODIFICHE
025200131003      *----
025300131007     c                   if        solo_una_volta = 'S'
025400131007     c                   eval            solo_una_volta = ' '
025500131003      *  UNB -
025600131008     c                   eval       Conta_righe = 1 + Conta_righe
025700131008      *
025800131003     c                   move      TA0017        TA6017            6
025900131007     c                   eval         STADAT = UNB_sgm
026000131007      *
026100131007      * sostituito chiodato l'UNB anche per provare
026200131007     c**********         eval         STADAT = %Trim(STADAT)
026300131007     c*********                     + %Trim(TA0010)
026400131007     c********           if             TAA007 <> *blank
026500131007     c*******            eval         STADAT = %Trim(STADAT)
026600131007     c********                      +   diviso
026700131007     c*********                     + %Trim(TAA007)
026800131007     c**********         end
026900131003      *
027000131003     c                   eval         STADAT = %Trim(STADAT)
027100131003     c                              +   piu
027200131003     c                              + %Trim(%editc(Data6:'X'))
027300131003     c                              +   diviso
027400131003     c                              + %Trim(%editc(Oraminuti:'X'))
027500131003     c                              +   piu
027600131007     c                              + %Trim(TAPRG)
027700131003     c                              +   apice
027800120830      *
027900120830     c                   write     IFTSTA00
028000131007     c                   endIF
028100131007      *----------------
028200131003      *  UNH -
028300131008     C                   eval      Conta_segm = 0
028400131008      *
028500131008     c                   eval       Conta_righe = 1 + Conta_righe
028600131008     c                   eval       Conta_segm  = 1 + Conta_segm
028700131008      *
028800131007     c                   eval      conta_UNH = conta_UNH + 1
028900131007     c                   eval         STADAT = UNH_sgm1
029000131007     c                              + %Trim(%Editc(conta_UNH:'Z'))
029100131007     c                              + UNH_sgm2
029200131007     c                              +   apice
029300131003      *
029400131003     c                   write     IFTSTA00
029500131003      *  BGM -
029600131008     c                   eval       Conta_righe = 1 + Conta_righe
029700131008     c                   eval       Conta_segm  = 1 + Conta_segm
029800131008      *
029900131003     c                   eval         STADAT = BGM_sgm
030000131003     c                              + %Trim(%Editc(ARBRMN:'Z'))
030100131003     c                              +   apice
030200131003      *
030300131003     c                   write     IFTSTA00
030400131003      *  NAD SF -
030500131008     c                   goto      NON_Necessario
030600131008     c                   eval       Conta_righe = 1 + Conta_righe
030700131008     c                   eval       Conta_segm  = 1 + Conta_segm
030800131008      *
030900131003     c                   eval         STADAT = NAD_SF_1
031000131003     c                              + NAD_SF_2
031100131003     c                              + NAD_SF_3
031200131003     c                              + NAD_SF_4
031300131003     c                              + NAD_SF_5
031400131003     c                              +   apice
031500131003      *
031600131003     c                   write     IFTSTA00
031700131008     c     NON_Necessariotag
031800131003      *  NAD ST -
031900131008     c                   eval       Conta_righe = 1 + Conta_righe
032000131008     c                   eval       Conta_segm  = 1 + Conta_segm
032100131008      *
032200131003     c                   eval         STADAT = NAD_ST
032300131003     c                              + %trim(arbRSD)
032400131003     c                              +   piu
032500131003     c                              +   piu
032600131003     c                              + %trim(arbIND)
032700131003     c                              +   piu
032800131003     c                              + %trim(arbLOD)
032900131003     c                              +   piu
033000131003     c                              + %trim(arbPRD)
033100131003     c                              +   piu
033200131003     c                              + %trim(arbCAD)
033300131003     c                              +   piu
033400131003     c                              + 'IT'
033500131003     c                              +   apice
033600131003      *
033700131003     c                   write     IFTSTA00
033800131003      *  LOC -
033900131008     c                   eval       Conta_righe = 1 + Conta_righe
034000131008     c                   eval       Conta_segm  = 1 + Conta_segm
034100131008      *
034200131003     c                   eval         STADAT = LOC_sgm1
034300131008     c                              + %Trim(%Editc(orgFIL:'X'))
034400131003     c                              +   LOC_sgm2
034500131008     c                              + %Trim(orgDES)
034600131008     c                              +   LOC_sgm3
034700131003     c                              +   apice
034800131003      *
034900131003     c                   write     IFTSTA00
035000131003      *----
035100131003      *  CNI -
035200131008     c                   eval       Conta_righe = 1 + Conta_righe
035300131008     c                   eval       Conta_segm  = 1 + Conta_segm
035400131008      *
035500131003     c                   eval         STADAT = CNI_sgm
035600131003     c                              +   apice
035700131003      *
035800131003     c                   write     IFTSTA00
035900131003      *  STS -
036000131008     c                   eval       Conta_righe = 1 + Conta_righe
036100131008     c                   eval       Conta_segm  = 1 + Conta_segm
036200131008      *
036300131003     c                   eval         STADAT = STS_sgm
036400131003     c                              + %Trim(STSSUBSTS)
036500131003     c                              +   apice
036600131003      *
036700131003     c                   write     IFTSTA00
036800131003      *  RFF -
036900131008     c                   eval       Conta_righe = 1 + Conta_righe
037000131008     c                   eval       Conta_segm  = 1 + Conta_segm
037100131008      *
037200131003     c                   eval         STADAT = RFF_sgm
037300131003     c                              + %trim(arbRMA)
037400131003     c                              +   apice
037500131003      *
037600131003     c                   write     IFTSTA00
037700131003      *  TDT -
037800131008     c                   eval       Conta_righe = 1 + Conta_righe
037900131008     c                   eval       Conta_segm  = 1 + Conta_segm
038000131008      *
038100131003     c                   eval         STADAT = TDT_sgm
038200131003     c                              +   apice
038300131003      *
038400131003     c                   write     IFTSTA00
038500131003      *  DTM -
038600131008     c                   eval       Conta_righe = 1 + Conta_righe
038700131008     c                   eval       Conta_segm  = 1 + Conta_segm
038800131008      *
038900131003     c                   eval         STADAT = DTM_sgm
039000131003     c                              + %Trim(DB2380)
039100131007     c                              + '00'
039200131003     c                              +   diviso
039300131007     c                              + '204'
039400131003     c                              +   apice
039500131003      *
039600131003     c                   write     IFTSTA00
039700131003      *  EQD -
039800131008     c                   eval       Conta_righe = 1 + Conta_righe
039900131008     c                   eval       Conta_segm  = 1 + Conta_segm
040000131008      *
040100131003     c                   eval         STADAT = EQD_sgm
040200131003     c                              +   apice
040300131003      *
040400131003     c                   write     IFTSTA00
040500131003      *  MEA -
040600131008     c                   eval       Conta_righe = 1 + Conta_righe
040700131008     c                   eval       Conta_segm  = 1 + Conta_segm
040800131008      *
040900131003     c                   eval         STADAT = MEA_sgm
041000131003     c                              + %Trim(%Editc(INTpes:'Z'))
041100131003     c                              + '.'
041200131003     c                              + %Editc(DECpes:'X')
041300131003     c                              + '0'
041400131003     c                              +   apice
041500131003      *
041600131003     c                   write     IFTSTA00
041700131003      *  GID -
041800131008     c                   eval       Conta_righe = 1 + Conta_righe
041900131008     c                   eval       Conta_segm  = 1 + Conta_segm
042000131008      *
042100131003     c                   eval         STADAT = GID_sgm
042200131003     c                              +   apice
042300131003      *
042400131003     c                   write     IFTSTA00
042500131003      *----
042600131003      *  UNT -
042700131008     c                   eval       Conta_segm  = 1 + Conta_segm
042800131008     c                   eval       Conta_righe = 1 + Conta_righe
042900131008      *
043000131003     c                   eval         STADAT = UNT_sgm
043100131008     c                              + %Trim(%Editc(conta_Segm:'Z'))
043200131003     c                              +   piu
043300131008     c                              + %Trim(%Editc(conta_UNH:'Z'))
043400131003     c                              +   apice
043500131003      *
043600131003     c                   write     IFTSTA00
043700131007      **********
043800131003     c                   endsr
043900131003      **?__________________________________________________________________ */
044000131007      **    Chiude il file IFTSTA da inviare
044100131003      **?__________________________________________________________________ */
044200131007     C     WRI_FINE      Begsr
044300131007      *
044400131007      * Scrive il record di chiusura del File
044500131007      *
044600131008     C                   IF          Conta_righe > 1
044700131007      *  UNZ -
044800131007     c                   eval         STADAT = UNZ_sgm
044900131007     c                              + %Trim(TAPRG)
045000131007     c                              +   apice
045100131007      *
045200131007     c                   eval       Conta_righe = 1 + Conta_righe
045300131007     c                   write     IFTSTA00
045400131008     c                   end
045500131007      *
045600131007     c                   endsr
045700131007      **?__________________________________________________________________ */
045800131007      **    tutto ciò che deve essere decodificato prima di Scrivere
045900131007      **?__________________________________________________________________ */
046000131007     C     DECODIFICHE   Begsr
046100131007      *
046200131003     C                   eval        DataOra    = DB2380
046300131003      *
046400131003      * DECODIFICHE da riportare nel MESSAGGIO
046500131003     c                   clear                   campo14a         14
046600131003     c                   eval      campo14a = %Trim(DA1004)
046700131003     c                   z-add     0             pos               3 0
046800131003     c                   eval      pos = %Check(digits:%trim(campo14a))
046900131003     c                   if        pos > 0
047000131003     c                   eval      arbRMN = *all'9'                             ci sono ALFA
047100131003     c                   else
047200131003     c                   move      campo14a      bolla14
047300131003     c                   z-add     2000          arbAAS
047400131003     c                   move      anno2         arbAAS
047500131003     c                   move      linea         arbLNP
047600131003     c                   move      serie         arbNRS
047700131003     c                   move      numspe        arbNSP
047800131003      *  aggancia la Bolla
047900131003     c     karb01        chain     fnARB01l
048000131003     c                   if        %Found(fnARB01l)
048100131003     c                   exsr      VOL_PES
048200131015     c     arbLNA        chain     AzORG01l
048300131008     c                   if        %Found(AzORG01l)
048400131008     c                   else
048500131008     c                   move      *zeros        orgFIl
048600131024     c                   eval      orgDES = 'X'
048700131008     c                   end
048800131003     c                   else
048900131003     c                   clear                   fnarb000
049000131003     c                   end
049100131003     c                   end
049200131003      *
049300131003     c*  conversione DEL CODICE STATUS.
049400131003     c*   STS solo il SOTTOCODICE x AMAZON (un unico numero)
049500131003     C                   eval        STSUNBCLI  = 'AMAZON'
049600131003     C                   eval        STSCODSTSB = DB9011
049700131003     C                   eval        STSSUBSTSB = DB9013
049800131003      **                 eval           STSSUBSTS = *Hival
049900131003     c     kedSTS        chain     edsts02l
050000131003     c***
050100131003     c                   if        %Found(edsts02l)
050200131003      *
050300131003     c                   if          DB9011 = '21 ' and DC3036 <> *blank
050400131003     c                   eval      campo_alfa = DC3036
050500131003     c                   exsr      Char_Speciali
050600131003     c                   eval      Firma  = campo_alfa
050700131003     c                   end
050800131003      *
050900131003     c                   end
051000131024      *    arbRSD
051100131024     c                   eval      campo_alfa = arbRSD
051200131024     c                   exsr      Char_Speciali
051300131024     c                   eval      arbRSD = campo_alfa
051400131024      *    arbIND
051500131024     c                   eval      campo_alfa = arbIND
051600131024     c                   exsr      Char_Speciali
051700131024     c                   eval      arbIND = campo_alfa
051800131024      *    arbLOD
051900131024     c                   eval      campo_alfa = arbLOD
052000131024     c                   exsr      Char_Speciali
052100131024     c                   eval      arbLOD = campo_alfa
052200131024      *    arbPRD
052300131024     c                   eval      campo_alfa = arbPRD
052400131024     c                   exsr      Char_Speciali
052500131024     c                   eval      arbPRD = campo_alfa
052600131024      *    arbCAD
052700131024     c                   eval      campo_alfa = arbCAD
052800131024     c                   exsr      Char_Speciali
052900131024     c                   eval      arbCAD = campo_alfa
053000131024      *    arbRMA
053100131024     c                   eval      campo_alfa = arbRMA
053200131024     c                   exsr      Char_Speciali
053300131024     c                   eval      arbRMA = campo_alfa
053400131024      *    orgDES
053500131024     c                   eval      campo_alfa = orgDES
053600131024     c                   exsr      Char_Speciali
053700131024     c                   eval      orgDES = campo_alfa
053800120830      *
053900120830     c                   endsr
054000131003      **?__________________________________________________________________ */
054100131003     C*  Reperisce Peso da CML o da Bollettato
054200131003      **?__________________________________________________________________ */
054300131003     C     VOL_PES       BEGSR
054400131003     C*
054500131003     C* PESO VDL -
054600131003     c     *like         define    arbpkb        wpes
054700131003     C                   Z-ADD     arbPKC        wpes
054800131003      *
054900131003      * Poi cerca il peso fra bollettato e CML
055000131003     C     arbncP        IFEQ      arbncl
055100131003      * se è considerata un'unica spedizione
055200131003     C                   ELSE
055300131003     C     wpes          iflt      arbPKb
055400131003     C                   Z-ADD     arbPKb        wpes
055500131003     C                   end
055600131003     C                   ENDIF
055700131003     C*
055800131003     C                   MOVE      WPES          DECPES            1 0
055900131003     C                   MOVEL     WPES          INTPES            6 0
056000131003     C*
056100131003     C                   ENDSR
056200120830      **?__________________________________________________________________ */
056300110405      **    FINTO ROLLBACK
056400120830      **?__________________________________________________________________ */
056500110405     C     ROLL_BCK      Begsr
056600110316      *
056700110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
056800110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
056900110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
057000110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
057100110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
057200110405      *
057300110405     C                   Endsr
057400120830      **?__________________________________________________________________ */
057500120830      **    Caratteri particolari  (; o ,) tipici separatori file CSV
057600120830      **?__________________________________________________________________ */
057700110523     C     Char_Speciali Begsr
057800110523      *
057900131003      *  Controlla la presenza di caratteri Speciali (campi strutturali)
058000131003      *   dichiarati nei segmenti    e li annulla
058100131003      *   la virgola viene trasformata in punto
058200120830      *
058300120830     c                   clear                   campo_ritorno   512
058400131003     c                   eval      campo_ritorno = %Xlate('+':' ':campo_alfa)
058500131024     c                   movel(p)  campo_ritorno campo_alfa      512
058600131003     c                   eval      campo_ritorno = %Xlate(':':' ':campo_alfa)
058700120830     c                   movel(p)  campo_ritorno campo_alfa
058800131003     c                   eval      campo_ritorno = %Xlate('?':' ':campo_alfa)
058900131003     c                   movel(p)  campo_ritorno campo_alfa
059000131003     c                   eval      campo_ritorno = %Xlate(',':'.':campo_alfa)
059100131003     c                   movel(p)  campo_ritorno campo_alfa
059200131024     c                   eval      campo_ritorno = %Xlate('''':' ':campo_alfa)
059300131024     c                   movel(p)  campo_ritorno campo_alfa
059400110523      *
059500110523     C                   Endsr
059600120830      **?__________________________________________________________________ */
059700120830      **    Prima del primo CNI valido scrive la TESTATA parcheggiata su EDIFTSTA
059800120830      **?__________________________________________________________________ */
059900120830     C     Copia_in_VGD  Begsr
060000121022      *
060100120830      * riporta sul TIVGD ciò che parcheggiato su EDIFTSTA
060200121022     c                   open      EDIFTSTA                             99
060300120830      *  poi copia
060400121022     c     *start        setll     EDIFTSTA
060500121022     c                   read      EDIFTSTA
060600120830     c                   dow       not %EoF(EDIFTSTA)
060700120830     c                   eval         segmento = staDAT
060800120830     C                   EXSR      VGD_WRITE
060900121022     c                   delete    IFTSTA00
061000121022     c                   read      EDIFTSTA
061100120830     c                   enddo
061200121022      * CHIUDE definitivamente
061300121022     c                   close     EDIFTSTA                             99
061400120830      *
061500120830     C                   Endsr
061600120830      **?__________________________________________________________________ */
061700120830      *    Apertura del TIVGD
061800120830      **?__________________________________________________________________ */
061900120830     C     VGD_OPEN      Begsr
062000120830      *
062100120830      *  recupera il progressivo
062200120830     c                   exsr      Piglia_progr
062300120830      *
062400120830      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
062500120830     C                   clear                   trul47ds
062600120830     C                   eval      d47opz  = 'I'
062700120830     C                   eval      d47tip  = TipoF
062800120830     C                   eval      d47lck  = 'N'
062900120830     C                   eval      d47chkj = 'N'
063000131003     C                   eval      d47pgm  = 'TRTCT81AG'
063100120830     C                   call      'TRUL47R'
063200120830     C                   parm                    trul47ds
063300120830      *
063400120830     c                   endsr
063500120830      **?__________________________________________________________________ */
063600120830      *   Scrittura del TIVGD
063700120830      **?__________________________________________________________________ */
063800120830     C     VGD_WRITE     Begsr
063900120830      *
064000120830     c                   clear                   tivgd000
064100120830     c                   eval      vgddta = %TrimR(Segmento)
064200120830     c                   eval      vgdtip = TipoF
064300120830     c                   eval      vgdksu = CLIENTE
064400120830     C                   eval      vgdprg = Progressivo
064500120830     c                   eval      vgdtsc = 'WW'
064600131003     c                   eval      vgdpgm = 'TRTCT81AG'
064700120830     c                   eval      vgddat = udtymd
064800120830      *
064900120830     C                   WRITE     tivgd000
065000120830      *
065100120830     c                   endsr
065200120830      **?__________________________________________________________________ */
065300120830      *    Chiusura del TIVGD
065400120830      **?__________________________________________________________________ */
065500120830     C     VGD_CLOSE     Begsr
065600120830      *
065700120830     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
065800120830     C                   clear                   trul47ds
065900120830     C                   eval      d47opz  = 'F'
066000120830     C                   eval      d47tip  = TipoF
066100120830     C                   call      'TRUL47R'
066200120830     C                   parm                    trul47ds
066300120830      *
066400120830     c                   endsr
066500110601      **?__________________________________________________________________ */
066600110617      *   Prende il Progressivo
066700110617      **?__________________________________________________________________ */
066800110617     C     Piglia_progr  Begsr
066900110617      *
067000110617     C                   MOVEL     CLIENTE       virKSC
067100110617     C                   MOVEL     TipoF         virTIP
067200110617     c     kvir02        chain     tivir02l
067300110617     c                   move      'OF'          Var_ISV
067400110617     c                   if        %Found(tivir02l)
067500110617     c                   move      VirFI1        Var_ISV
067600110617     C                   End
067700110617
067800110617      *       prende numeratore strategi
067900110617     C                   exsr      calprog
068000110617      *
068100110617     c                   endsr
068200120830      **?__________________________________________________________________ */
068300110617     C*  prepara il file
068400120830      **?__________________________________________________________________ */
068500110617     C     calprog       begsr
068600110617     C*
068700110617     C                   open      tis7prgf
068800110617     C                   read(e)   tis7prgf
068900110617     C*
069000110617     C                   if        not %error
069100110617     C                   eval      dwlprg = f_tis7prgf
069200110617     C*
069300110617     C                   move(p)   dwlprg        wrkprg
069400110617     C                   add       1             wrkprg
069500110617     C                   move(p)   wrkprg        dwlprg
069600110617     C                   movel     Var_ISV       dwlprg
069700110617     C*
069800110617     C                   eval       f_tis7prgf = dwlprg
069900110617     C                   eval      Progressivo = dwlprg
070000110617     C*
070100110617     C                   update    tis7prg0
070200110617     C                   endif
070300110617     C*
070400110617     C                   close     tis7prgf
070500110617     C*
070600110617     C                   endsr
070700110617      **?__________________________________________________________________ */
070800110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
070900120830      **?__________________________________________________________________ */
071000110621     C     *pssr         BEGSR
071100110621     C                   eval      esito = 'E'
071200110621     C                   ENDSR     '*CANCL'
071300120830      **?__________________________________________________________________ */
