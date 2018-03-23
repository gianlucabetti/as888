000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200120830     H* TRTCT81RG ---------------------------------------------------*
000300900418     H*                                                              *
000400120830     H* Scrittura del file nle formato NET EXPRESS EUROPE  x GEL     *
000500120920     H* T;DE1;Barcode;Rif.Sped.;Status;ED2;Data;Ora;Firma/Nota;;;    *
000600120830     H*                                                              *
000700120830     H*  Utilizza il EDIFTSTA in QTEMP come Workfile generico        *
000800120830     H*--------------------------------------------------------------*
000900121022     FEDIFTSTA  uF A E             DISK    usropn
001000120830     FEDsts02L  iF   E           K DISK
001100120830      **?__________________________________________________________________ */
001200120830     ftivgd00f  if a E             DISK    usropn commit
001300110617     Ftivir02L  IF   E           K DISK
001400110617     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001500110617     F                                     PREFIX(f_) usropn
001600110617      **?__________________________________________________________________ */
001700120830     D Var_ISV         s                   like(Virfi1)
001800110617     D TipoF           C                   CONST('EA')
001900110617     D dwlprg          s             10    INZ(*all'0')
002000110617     D wrkprg          s              8  0 INZ(*zeros)
002100060814     d trul47ds      e ds
002200120830      *
002300120830     D DataOra         DS            12
002400120830     D Data                           8
002500120830     D Ora                            4
002600120830      *
002700120830     d  Firma          s             20
002800110316     D Segmento        s           2048
002900110316     D Conta_righe     s              5i 0
003000110621     d  esito          s              1
003100120830      **?__________________________________________________________________ */
003200110316     D* DS
003300120830      **?__________________________________________________________________ */
003400110316     D* .. per scompozione dati da spedire a seconda del tipo record
003500120830     D SD00ds        E DS                  EXTNAME(EDSD00DS)
003600120830     D SD05ds        E DS                  EXTNAME(EDSD05DS)
003700110316     D SD10ds        E DS                  EXTNAME(EDSD10DS)
003800110316     D SD15ds        E DS                  EXTNAME(EDSD15DS)
003900110316     D  D15                   13    362
004000110316     D                                     DIM(5)
004100110316     D SD20ds        E DS                  EXTNAME(EDSD20DS)
004200110316     D  D20blk                10     22
004300110316     D  D20                   33    732
004400110316     D                                     DIM(20)
004500120830      **?__________________________________________________________________ */
004600120920     D Inizio          C                   CONST('T;DE1;')
004700120830     D Fine            C                   CONST(';;;')
004800120830     D PVirgola        C                   CONST(';')
004900120830      **?__________________________________________________________________ */
005000110405     I* FILE
005100120830      **?__________________________________________________________________ */
005200110405     IIFTSTA00
005300110405     I              SSIFTSTA                    staDAT
005400050927      **?__________________________________________________________________ */
005500050704      *    main
005600110316      **?__________________________________________________________________ */
005700110316     c     *entry        plist                                                   esempio
005800110316     C                   PARM                    CLIENTE           8            "03400002"
005900110316     C                   PARM                    Tipo_DS           4            "ST00"
006000110316     C                   PARM                    DsGenerica     1024             EDST00DS
006100110601     C                   PARM                    Numero_Righe      5 0          nnnnn
006200110617     C                   PARM                    Progressivo      10
006300110621     c                   PARM                    esito             1
006400110617     c*
006500110617     c     kvir02        klist
006600110617     C                   kfld                    virKSC
006700110617     C                   kfld                    virTIP
006800120830     c*
006900120830     c     kedSTS        klist
007000120830     C                   kfld                    STSUNBCLI
007100120830     C                   kfld                    STSCODSTSB
007200120830     C                   kfld                    STSSUBSTSB
007300110322     c*
007400110322     C                   z-add     Numero_Righe  Conta_righe
007500110316     c*
007600110316     C                   TIME                    ORADAT           14 0
007700110316     c                   move      *date         udtymd            8 0
007800120830     C                   TIME                    W0140            14 0
007900110316     C                   MOVE      W0140         W0080             8 0
008000120830     c                   move      W0140         dati_Oramin      14
008100120830
008200120830      * salva la testat in QTEMP
008300120830     c                   open      EDIFTSTA                             99
008400050927      **?__________________________________________________________________ */
008500110316      * Decodifica la DS in base ai Parametri
008600110316      **?__________________________________________________________________ */
008700110316     c                   select
008800110316
008900110405      * da qui inizia il dettaglio
009000110405     c                   when      Tipo_DS = 'SD00'
009100110316     c                   eval        SD00ds = DsGenerica
009200120830      *   STS RFF DTM
009300110316     c                   when      Tipo_DS = 'SD05'
009400110316     c                   eval        SD05ds = DsGenerica
009500120830      *   NAD
009600110316     c                   when      Tipo_DS = 'SD10'
009700110316     c                   eval        SD10ds = DsGenerica
009800120830      *   FTX
009900110316     c                   when      Tipo_DS = 'SD15'
010000110316     c                   eval        SD15ds = DsGenerica
010100120830      *   GID PCI
010200110316     c                   when      Tipo_DS = 'SD20'
010300110316     c                   eval        SD20ds = DsGenerica
010400120830      *
010500120830      **   scrive max.20 righe quanti i codici a barre contenuti
010600120830     c                   do        20            X                 3 0
010700120830     c                   if        D20(x)  = *blank
010800120830     c                   leave
010900120830     c                   end
011000120830      **
011100120830     c                   movel     D20(x)        BARcode          35
011200120830     c                   eval      campo_alfa  = BARcode
011300120830     c                   exsr      Char_Speciali
011400120830     c                   eval      BARcode     = campo_alfa
011500120830      **
011600120830     c                   exsr      SAV_IFTSTA
011700120830     c                   enddo
011800110316
011900110316     c                   when      Tipo_DS = 'FINE'
012000120830
012100120830      * dal primo record scritto  CHIUDE E RIAPRE in Qtemp
012200121022     c                   close     EDIFTSTA                             99
012300120830      * Apre
012400120830      *   il TIVGD
012500121022     c                   open      TIVGD00F                             99
012600120830     C                   EXSR      vgd_OPEN
012700120830      * Lo scrive
012800120830     C                   EXSR      copia_in_VGD
012900120830      * quindi lo chiude
013000120830     C                   EXSR      vgd_CLOSE
013100120830     c                   close     TIVGD00F                             99
013200120830
013300110405      * conferma la scrittura del TIVGD
013400110405     c                   commit
013500121022      *
013600121022     C                   z-add     Conta_righe   Numero_Righe
013700121022     c                   seton                                        LR
013800121022     c                   RETURN
013900110405
014000110405      * se servisse gestire qualcosa come un "ROLLBACK" FITTIZIO
014100110405      *  si deve impostare in questo punto del programma.
014200110405     c                   when      Tipo_DS = 'RLBK'
014300110405     c                   exsr      ROLL_BCK
014400121022      *
014500121022     C                   z-add     Conta_righe   Numero_Righe
014600121022     c                   seton                                        LR
014700121022     c                   RETURN
014800110316
014900110316     C                   ENDSL
015000050707      *
015100110322      *  per poter contare in totale le righe deve ripassare a che punto è arrivato
015200110322     C                   z-add     Conta_righe   Numero_Righe
015300110322      *
015400121022     c                   seton                                        RT
015500121022     c                   RETURN
015600120830      **?__________________________________________________________________ */
015700120830      **    Scrittura  righe di testata in QTEMP nel EDIFTSTA
015800120830      **?__________________________________________________________________ */
015900120830     C     SAV_IFTSTA    Begsr
016000120830      *
016100120830     c                   eval       Conta_righe = 1 + Conta_righe
016200120830     c*
016300120830     c*  se riuscita la conversione invia il record Altrimenti non lo manda.
016400120830     C                   eval        STSUNBCLI  = 'GEL_DE'
016500120830     C                   eval        STSCODSTSB = DB9011
016600120830     C                   eval        STSSUBSTSB = DB9013
016700120830     c     kedSTS        chain     edsts02l
016800120830     c                   if        %Found(edsts02l)
016900120830      *
017000120830     C                   eval        DataOra    = DB2380
017100120830     c                   clear                   Ora5              5
017200120830     c                   if        Ora <> *blank
017300120830     c                   eval      Ora5= %subst(Ora:1:2) +':'+ %subst(Ora:3:2)
017400120830     c                   end
017500120830      *
017600120830     c                   if          STSSUBSTS = 'OK' and DB9011 = '21 '
017700120830     c                               and   DC3036 <> *blank
017800120830     c                   eval      campo_alfa = DC3036
017900120830     c                   exsr      Char_Speciali
018000120830     c                   eval      Firma  = campo_alfa
018100120830     c                   end
018200120830      *
018300120830      *   Compone la riga nel formato Net Express da memorizzare su EDIFTSTA in QTEMP
018400120830     c                   eval         STADAT =
018500120830     c                                  Inizio
018600120830     c                                + %Trim(BARcode)
018700120830     c                                + PVirgola
018800120830     c                                + %Trim(DB1154)
018900120830     c                                + PVirgola
019000120830     c                                + %Trim(STSSUBSTS)
019100120830     c                                + PVirgola
019200120830     c                                + %Trim(STSCODSTS)
019300120830     c                                + PVirgola
019400120830     c                                + %Trim(Data)
019500120830     c                                + PVirgola
019600120830     c                                + %Trim(Ora5)
019700120830     c                                + PVirgola
019800120830     c                                + %Trim(Firma)
019900120830     c                                + Fine
020000120830      *
020100120830     c                   write     IFTSTA00
020200120830     c                   endif
020300120830      *
020400120830     c                   endsr
020500120830      **?__________________________________________________________________ */
020600110405      **    FINTO ROLLBACK
020700120830      **?__________________________________________________________________ */
020800110405     C     ROLL_BCK      Begsr
020900110316      *
021000110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
021100110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
021200110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
021300110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
021400110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
021500110405      *
021600110405     C                   Endsr
021700120830      **?__________________________________________________________________ */
021800120830      **    Caratteri particolari  (; o ,) tipici separatori file CSV
021900120830      **?__________________________________________________________________ */
022000110523     C     Char_Speciali Begsr
022100110523      *
022200110523      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
022300120830      *   dichiarati nei segmenti    (;,)
022400120830      *   [virgola o punto e virgola] caratteri separatori dei file CSV
022500120830      *
022600120830     c                   clear                   campo_ritorno   512
022700120830     c                   eval      campo_ritorno = %Xlate(';':'-':campo_alfa)
022800110523     c                   movel(p)  campo_ritorno campo_alfa      512
022900120830     c                   eval      campo_ritorno = %Xlate(',':'-':campo_alfa)
023000120830     c                   movel(p)  campo_ritorno campo_alfa
023100110523      *
023200110523     C                   Endsr
023300120830      **?__________________________________________________________________ */
023400120830      **    Prima del primo CNI valido scrive la TESTATA parcheggiata su EDIFTSTA
023500120830      **?__________________________________________________________________ */
023600120830     C     Copia_in_VGD  Begsr
023700121022      *
023800120830      * riporta sul TIVGD ciò che parcheggiato su EDIFTSTA
023900121022     c                   open      EDIFTSTA                             99
024000120830      *  poi copia
024100121022     c     *start        setll     EDIFTSTA
024200121022     c                   read      EDIFTSTA
024300120830     c                   dow       not %EoF(EDIFTSTA)
024400120830     c                   eval         segmento = staDAT
024500120830     C                   EXSR      VGD_WRITE
024600121022     c                   delete    IFTSTA00
024700121022     c                   read      EDIFTSTA
024800120830     c                   enddo
024900121022      * CHIUDE definitivamente
025000121022     c                   close     EDIFTSTA                             99
025100120830      *
025200120830     C                   Endsr
025300120830      **?__________________________________________________________________ */
025400120830      *    Apertura del TIVGD
025500120830      **?__________________________________________________________________ */
025600120830     C     VGD_OPEN      Begsr
025700120830      *
025800120830      *  recupera il progressivo
025900120830     c                   exsr      Piglia_progr
026000120830      *
026100120830      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
026200120830     C                   clear                   trul47ds
026300120830     C                   eval      d47opz  = 'I'
026400120830     C                   eval      d47tip  = TipoF
026500120830     C                   eval      d47lck  = 'N'
026600120830     C                   eval      d47chkj = 'N'
026700120830     C                   eval      d47pgm  = 'TRTCT81RG'
026800120830     C                   call      'TRUL47R'
026900120830     C                   parm                    trul47ds
027000120830      *
027100120830     c                   endsr
027200120830      **?__________________________________________________________________ */
027300120830      *   Scrittura del TIVGD
027400120830      **?__________________________________________________________________ */
027500120830     C     VGD_WRITE     Begsr
027600120830      *
027700120830     c                   clear                   tivgd000
027800120830     c                   eval      vgddta = %TrimR(Segmento)
027900120830     c                   eval      vgdtip = TipoF
028000120830     c                   eval      vgdksu = CLIENTE
028100120830     C                   eval      vgdprg = Progressivo
028200120830     c                   eval      vgdtsc = 'WW'
028300120830     c                   eval      vgdpgm = 'TRTCT81RG'
028400120830     c                   eval      vgddat = udtymd
028500120830      *
028600120830     C                   WRITE     tivgd000
028700120830      *
028800120830     c                   endsr
028900120830      **?__________________________________________________________________ */
029000120830      *    Chiusura del TIVGD
029100120830      **?__________________________________________________________________ */
029200120830     C     VGD_CLOSE     Begsr
029300120830      *
029400120830     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
029500120830     C                   clear                   trul47ds
029600120830     C                   eval      d47opz  = 'F'
029700120830     C                   eval      d47tip  = TipoF
029800120830     C                   call      'TRUL47R'
029900120830     C                   parm                    trul47ds
030000120830      *
030100120830     c                   endsr
030200110601      **?__________________________________________________________________ */
030300110617      *   Prende il Progressivo
030400110617      **?__________________________________________________________________ */
030500110617     C     Piglia_progr  Begsr
030600110617      *
030700110617     C                   MOVEL     CLIENTE       virKSC
030800110617     C                   MOVEL     TipoF         virTIP
030900110617     c     kvir02        chain     tivir02l
031000110617     c                   move      'OF'          Var_ISV
031100110617     c                   if        %Found(tivir02l)
031200110617     c                   move      VirFI1        Var_ISV
031300110617     C                   End
031400110617
031500110617      *       prende numeratore strategi
031600110617     C                   exsr      calprog
031700110617      *
031800110617     c                   endsr
031900120830      **?__________________________________________________________________ */
032000110617     C*  prepara il file
032100120830      **?__________________________________________________________________ */
032200110617     C     calprog       begsr
032300110617     C*
032400110617     C                   open      tis7prgf
032500110617     C                   read(e)   tis7prgf
032600110617     C*
032700110617     C                   if        not %error
032800110617     C                   eval      dwlprg = f_tis7prgf
032900110617     C*
033000110617     C                   move(p)   dwlprg        wrkprg
033100110617     C                   add       1             wrkprg
033200110617     C                   move(p)   wrkprg        dwlprg
033300110617     C                   movel     Var_ISV       dwlprg
033400110617     C*
033500110617     C                   eval       f_tis7prgf = dwlprg
033600110617     C                   eval      Progressivo = dwlprg
033700110617     C*
033800110617     C                   update    tis7prg0
033900110617     C                   endif
034000110617     C*
034100110617     C                   close     tis7prgf
034200110617     C*
034300110617     C                   endsr
034400110617      **?__________________________________________________________________ */
034500110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
034600120830      **?__________________________________________________________________ */
034700110621     C     *pssr         BEGSR
034800110621     C
034900110621     C                   eval      esito = 'E'
035000110621     C                   ENDSR     '*CANCL'
035100110621     C
035200120830      **?__________________________________________________________________ */
