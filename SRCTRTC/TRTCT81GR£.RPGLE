000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200120830     H* TRTCT81RG ---------------------------------------------------*
000300900418     H*                                                              *
000400120830     H* Scrittura del file nle formato NET EXPRESS EUROPE  x GEL     *
000500120920     H* T;DE1;Barcode;Rif.Sped.;Status;ED2;Data;Ora;Firma/Nota;;;    *
000600120830     H*                                                              *
000700120830     H*  Utilizza il EDIFTSTA in QTEMP come Workfile generico        *
000800120830     H*--------------------------------------------------------------*
000900110405     FEDIFTSTA  iF A E             DISK    usropn
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
012200120830     c                   close     EDIFTSTA                             99
012300120830      * Apre
012400120830      *   il TIVGD
012500120830     c                   open      TIVGD00F                             99
012600120830     C                   EXSR      vgd_OPEN
012700120830      * Lo scrive
012800120830     C                   EXSR      copia_in_VGD
012900120830      * quindi lo chiude
013000120830     C                   EXSR      vgd_CLOSE
013100120830     c                   close     TIVGD00F                             99
013200120830
013300110405      * conferma la scrittura del TIVGD
013400110405     c                   commit
013500110405
013600110405      * se servisse gestire qualcosa come un "ROLLBACK" FITTIZIO
013700110405      *  si deve impostare in questo punto del programma.
013800110405     c                   when      Tipo_DS = 'RLBK'
013900110405     c                   exsr      ROLL_BCK
014000110316
014100110316     C                   ENDSL
014200050707      *
014300110322      *  per poter contare in totale le righe deve ripassare a che punto è arrivato
014400110322     C                   z-add     Conta_righe   Numero_Righe
014500110322      *
014600120830     c                   seton                                        RT
014700120830      **?__________________________________________________________________ */
014800120830      **    Scrittura  righe di testata in QTEMP nel EDIFTSTA
014900120830      **?__________________________________________________________________ */
015000120830     C     SAV_IFTSTA    Begsr
015100120830      *
015200120830     c                   eval       Conta_righe = 1 + Conta_righe
015300120830     c*
015400120830     c*  se riuscita la conversione invia il record Altrimenti non lo manda.
015500120830     C                   eval        STSUNBCLI  = 'GEL_DE'
015600120830     C                   eval        STSCODSTSB = DB9011
015700120830     C                   eval        STSSUBSTSB = DB9013
015800120830     c     kedSTS        chain     edsts02l
015900120830     c                   if        %Found(edsts02l)
016000120830      *
016100120830     C                   eval        DataOra    = DB2380
016200120830     c                   clear                   Ora5              5
016300120830     c                   if        Ora <> *blank
016400120830     c                   eval      Ora5= %subst(Ora:1:2) +':'+ %subst(Ora:3:2)
016500120830     c                   end
016600120830      *
016700120830     c                   if          STSSUBSTS = 'OK' and DB9011 = '21 '
016800120830     c                               and   DC3036 <> *blank
016900120830     c                   eval      campo_alfa = DC3036
017000120830     c                   exsr      Char_Speciali
017100120830     c                   eval      Firma  = campo_alfa
017200120830     c                   end
017300120830      *
017400120830      *   Compone la riga nel formato Net Express da memorizzare su EDIFTSTA in QTEMP
017500120830     c                   eval         STADAT =
017600120830     c                                  Inizio
017700120830     c                                + %Trim(BARcode)
017800120830     c                                + PVirgola
017900120830     c                                + %Trim(DB1154)
018000120830     c                                + PVirgola
018100120830     c                                + %Trim(STSSUBSTS)
018200120830     c                                + PVirgola
018300120830     c                                + %Trim(STSCODSTS)
018400120830     c                                + PVirgola
018500120830     c                                + %Trim(Data)
018600120830     c                                + PVirgola
018700120830     c                                + %Trim(Ora5)
018800120830     c                                + PVirgola
018900120830     c                                + %Trim(Firma)
019000120830     c                                + Fine
019100120830      *
019200120830     c                   write     IFTSTA00
019300120830     c                   endif
019400120830      *
019500120830     c                   endsr
019600120830      **?__________________________________________________________________ */
019700110405      **    FINTO ROLLBACK
019800120830      **?__________________________________________________________________ */
019900110405     C     ROLL_BCK      Begsr
020000110316      *
020100110405      *  E' meglio non gestire un ROLLBACK FISICO sul TIVGD poichè il file
020200110405      *  è troppo delicato nella sua funzione di DOWNLOAD.
020300110405      *   Se NON SI DEVE scrivere nulla poichè non si è rilevato nessuno stato
020400110405      *   da inviare al cliente, al momento si esce dal pgm senza aver fatto nulla
020500110405      *   Altrimenti questa è la routine per gestire qualche azione da compiere.
020600110405      *
020700110405     C                   Endsr
020800120830      **?__________________________________________________________________ */
020900120830      **    Caratteri particolari  (; o ,) tipici separatori file CSV
021000120830      **?__________________________________________________________________ */
021100110523     C     Char_Speciali Begsr
021200110523      *
021300110523      *  Controlla su campi alfanumerici la presenza di caratteri Speciali
021400120830      *   dichiarati nei segmenti    (;,)
021500120830      *   [virgola o punto e virgola] caratteri separatori dei file CSV
021600120830      *
021700120830     c                   clear                   campo_ritorno   512
021800120830     c                   eval      campo_ritorno = %Xlate(';':'-':campo_alfa)
021900110523     c                   movel(p)  campo_ritorno campo_alfa      512
022000120830     c                   eval      campo_ritorno = %Xlate(',':'-':campo_alfa)
022100120830     c                   movel(p)  campo_ritorno campo_alfa
022200110523      *
022300110523     C                   Endsr
022400120830      **?__________________________________________________________________ */
022500120830      **    Prima del primo CNI valido scrive la TESTATA parcheggiata su EDIFTSTA
022600120830      **?__________________________________________________________________ */
022700120830     C     Copia_in_VGD  Begsr
022800120830      *
022900120830      * riporta sul TIVGD ciò che parcheggiato su EDIFTSTA
023000120830     c                   open      EDIFTSTA                             99
023100120830      *  poi copia
023200120830     c                   read      EDIFTSTA
023300120830     c                   dow       not %EoF(EDIFTSTA)
023400120830     c                   eval         segmento = staDAT
023500120830     C                   EXSR      VGD_WRITE
023600120830     c                   read      EDIFTSTA
023700120830     c                   enddo
023800120830      *
023900120830     C                   Endsr
024000120830      **?__________________________________________________________________ */
024100120830      *    Apertura del TIVGD
024200120830      **?__________________________________________________________________ */
024300120830     C     VGD_OPEN      Begsr
024400120830      *
024500120830      *  recupera il progressivo
024600120830     c                   exsr      Piglia_progr
024700120830      *
024800120830      *  istruzioni apertura blocco scrittura TIVGD    Edi Standard
024900120830     C                   clear                   trul47ds
025000120830     C                   eval      d47opz  = 'I'
025100120830     C                   eval      d47tip  = TipoF
025200120830     C                   eval      d47lck  = 'N'
025300120830     C                   eval      d47chkj = 'N'
025400120830     C                   eval      d47pgm  = 'TRTCT81RG'
025500120830     C                   call      'TRUL47R'
025600120830     C                   parm                    trul47ds
025700120830      *
025800120830     c                   endsr
025900120830      **?__________________________________________________________________ */
026000120830      *   Scrittura del TIVGD
026100120830      **?__________________________________________________________________ */
026200120830     C     VGD_WRITE     Begsr
026300120830      *
026400120830     c                   clear                   tivgd000
026500120830     c                   eval      vgddta = %TrimR(Segmento)
026600120830     c                   eval      vgdtip = TipoF
026700120830     c                   eval      vgdksu = CLIENTE
026800120830     C                   eval      vgdprg = Progressivo
026900120830     c                   eval      vgdtsc = 'WW'
027000120830     c                   eval      vgdpgm = 'TRTCT81RG'
027100120830     c                   eval      vgddat = udtymd
027200120830      *
027300120830     C                   WRITE     tivgd000
027400120830      *
027500120830     c                   endsr
027600120830      **?__________________________________________________________________ */
027700120830      *    Chiusura del TIVGD
027800120830      **?__________________________________________________________________ */
027900120830     C     VGD_CLOSE     Begsr
028000120830      *
028100120830     C* Infine elimino il blocco elaborazione TIVGD    Edi Standard
028200120830     C                   clear                   trul47ds
028300120830     C                   eval      d47opz  = 'F'
028400120830     C                   eval      d47tip  = TipoF
028500120830     C                   call      'TRUL47R'
028600120830     C                   parm                    trul47ds
028700120830      *
028800120830     c                   endsr
028900110601      **?__________________________________________________________________ */
029000110617      *   Prende il Progressivo
029100110617      **?__________________________________________________________________ */
029200110617     C     Piglia_progr  Begsr
029300110617      *
029400110617     C                   MOVEL     CLIENTE       virKSC
029500110617     C                   MOVEL     TipoF         virTIP
029600110617     c     kvir02        chain     tivir02l
029700110617     c                   move      'OF'          Var_ISV
029800110617     c                   if        %Found(tivir02l)
029900110617     c                   move      VirFI1        Var_ISV
030000110617     C                   End
030100110617
030200110617      *       prende numeratore strategi
030300110617     C                   exsr      calprog
030400110617      *
030500110617     c                   endsr
030600120830      **?__________________________________________________________________ */
030700110617     C*  prepara il file
030800120830      **?__________________________________________________________________ */
030900110617     C     calprog       begsr
031000110617     C*
031100110617     C                   open      tis7prgf
031200110617     C                   read(e)   tis7prgf
031300110617     C*
031400110617     C                   if        not %error
031500110617     C                   eval      dwlprg = f_tis7prgf
031600110617     C*
031700110617     C                   move(p)   dwlprg        wrkprg
031800110617     C                   add       1             wrkprg
031900110617     C                   move(p)   wrkprg        dwlprg
032000110617     C                   movel     Var_ISV       dwlprg
032100110617     C*
032200110617     C                   eval       f_tis7prgf = dwlprg
032300110617     C                   eval      Progressivo = dwlprg
032400110617     C*
032500110617     C                   update    tis7prg0
032600110617     C                   endif
032700110617     C*
032800110617     C                   close     tis7prgf
032900110617     C*
033000110617     C                   endsr
033100110617      **?__________________________________________________________________ */
033200110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
033300120830      **?__________________________________________________________________ */
033400110621     C     *pssr         BEGSR
033500110621     C
033600110621     C                   eval      esito = 'E'
033700110621     C                   ENDSR     '*CANCL'
033800110621     C
033900120830      **?__________________________________________________________________ */
