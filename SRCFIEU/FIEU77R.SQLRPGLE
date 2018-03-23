000100030620     ************************************************************************
000200030624      *   Genera Record di controllo Fatture Partners Estero                *
000300161298      ***********************************************************************
000400080416     H DECEDIT('0,') DATEDIT(*YMD/)
000500041103      ***********************************************************************
000600130822     FAZORG01L  IF   E           K DISK
000700040909     Ffieu77D   CF   E             WORKSTN
000800080416     Ftnecx00f  uf a E           k disk    prefix(ECX_)
000900040909      *****************************************************************
001000030620      *
001100030624     D KPJBA         E DS
001200030625     D OG143         E DS
001300040909     D*
001400040909     D WLBDAT          DS
001500040909     D  G02DAT                 1      8  0
001600040909     D  G02INV                 9     16  0
001700040909     D  G02ERR                17     17
001800040909     D  G02TGI                18     22  0
001900040909      *
002000030625     D parDate         DS
002100030625     D   DaData                1      8  0
002200030625     D    aData                9     16  0
002300030625     D    aAnno                9     12  0
002400030625     D    aMesG               13     16  0
002500030703     D   TpNtw                17     19
002600030703     D   TpData               20     20
002700030625      *
002800030625     D parBolla        DS
002900030625     D   tasAAS                1      4  0
003000030625     D   tasLnp                5      7  0
003100030625     D   tasNrs                8      9  0
003200030625     D   tasNsp               10     16  0
003300030625     D   tasTbl               17     18
003400030703     D   KNtw                 19     21
003500041013      *
003600080416     D UltimaData      s              8  0
003700080416     D UltimaData_X    s              8  0
003800080416     D data_Iso        s               d   datfmt(*ISO)
003900080416     D d1NTWsav        s                   like(d1Ntw)
004000130822     D digits          c                   '0123456789'
004100030620      *---------------------------------------------------------------------*
004200030624     C     *entry        plist
004300030624     C                   parm                    kpjba
004400040909      **
004500041015      ** prima di attivare il programma controlla allineamento della
004600041015      **  tabella TNEVA con la tabella delle VARIE "CC"
004700041015      **    Se disallineata avvisa ed esce dal programma senza importare
004800041015      **     i dati.
004900041015     c                   exsr      CTR_tab_EVA
005000041015     c                   if        tab_noOK ='S'
005100041015     c                   exfmt     eu77d02
005200041015     C                   GOTO      FINE                                         F3=USCITA
005300041015     c                   end
005400041015      **
005500130822     c                   setoff                                       2829
005600041013      **
005700040909     C     INIZIO        tag
005800040909     c                   clear                   D1NTW
005900080416     c                   clear                   D1NTWsav
006000040909     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
006100040909     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
006200050419     C                   move      'B'           D1tip                          tipo elaborazione
006300040909      *
006400040909      * Emetto il formato video
006500040909     C     Video1        TAG
006600080416     c                   eval      d1NtwSav = d1Ntw
006700150414     C                   eval      D1DATE = 'Data Spediz.'                      tipo di date
006800150414      * x DPD
006900150414     c                   if        d1Ntw = 'DPD'
007000150414     C                   eval      D1DATE = 'Data Consegna'                     tipo di date
007100150414     c                   end
007200150414      *
007300040909     C                   SETOFF                                       90
007400040909     C                   EXFMT     eu77d01
007500040909      *
007600040909     C   KC              GOTO      FINE                                         F3=USCITA
007700040909      *
007800040909      * Eseguo i  controlli sui campi immessi a video
007900040909     C                   EXSR      CONTR
008000040909      *
008100040909      * Se non vi sono errori ma non e' premuto F6 emetto video
008200040909     C   90
008300040909     CorNkf              GOTO      Video1
008400040909      *
008500040909     C     Video2        TAG
008600040909     C                   SETOFF                                       90
008700040909      *
008800041013      *  Se scelti tutti
008900041013     c                   if        d1ntw = '999'
009000130822     c                   move      'DPD'         d1ntw
009100130822     C                   EXSR      LANCIO
009200130822     c                   move      'FED'         d1ntw
009300130822     C                   EXSR      LANCIO
009400130822     c     *loval        setll     azorg01l
009500130822     c                   read      azorg01l
009600130822     c                   dow       not %Eof(azorg01l)
009700130822     c                   eval      og143 = orgde3
009800130822     c                   if        §OGntw = 'EEX'
009900130822     c                   move      orgfil        d1ntw
010000041013     C                   EXSR      LANCIO
010100130822     c                   end
010200041013     c                   enddo
010300041013      *
010400041013     c                   else
010500041013      *
010600041013      * ESEGUE il caricamento
010700041013     C                   EXSR      LANCIO
010800041013     c                   end
010900041013      *
011000161298      *----------------------------------------------------------------
011100040909     c     FINE          tag
011200030620     c                   seton                                        LR
011300040909      *--------------------------------------------------------------*
011400040909      * Routine per i controlli sui campi del formato video          *
011500040909      *--------------------------------------------------------------*
011600040909     C     CONTR         BEGSR
011700040909      *
011800040909     c                   movea     '0000000000'  *in(41)
011900040909     c                   movea     '0000000000'  *in(51)
012000080416      * --------------
012100080416      * controllo il Network immesso
012200040909     C     d1ntw         ifeq      *blank
012300040909     C                   SETON                                        4190
012400040909     C                   GOTO      endct1
012500080416     c                   else
012600130822      *
012700130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
012800130822      * controlla la LNP se network EDI estero
012900130822     c                   z-add     0             dove              3 0
013000130822     c     digits        check     d1ntw         dove
013100130822     c                   if        dove > 0
013200130822      * ci sono alfanumerici
013300130822     C                   SETON                                        4190
013400130822     C                   GOTO      endct1
013500130822     C                   Else
013600130822     c                   move      d1ntw         ntw_num3          3 0
013700130822     c     ntw_num3      chain     azorg01l
013800130822     c                   if        not %Found(azorg01l)
013900130822     C                   SETON                                        4190
014000130822     C                   GOTO      endct1
014100130822     C                   Else
014200130822     c                   eval      og143 = orgde3
014300130822     c                   if        §ogNTW <> 'EEX'
014400130822     C                   SETON                                        4190
014500130822     C                   GOTO      endct1
014600130822     c                   end
014700130822     c                   end
014800130822     C                   End
014900130822      *
015000130822     C                   End
015100130822     C                   End
015200040909      * --------------
015300080416      * controllo immissione dalla data o la protegge
015400080416      * --------------
015500080416     c                   If        d1NtwSav <> d1Ntw
015600080416     c                   clear                   UltimaData
015700080416     C/EXEC SQL
015800080416     C+ SELECT max(ECFAAS*10000+ecfmgs) INTO :UltimaData
015900080416     C+ FROM tnecf00f WHERE ecfntw= :d1ntw
016000080416     C/END-EXEC
016100080416     c                   setoff                                       28
016200080416     c                   exsr      ctrl_estratto
016300080416      *
016400080416      * Al primo cambio di Network  Pulisce le date e riemette il video
016500080416     c                   if        sqlcod < 0  or UltimaData = 0
016600080416     c                   clear                   d1dDal
016700080416     c                   clear                   d1Dal
016800080416     c                   goto      endct1
016900080416     c                   end
017000080416      *
017100080416      * protegge se presenti già dei dati sul Network
017200080416     c                   if        UltimaData > 0
017300080416     c     *iso          move      UltimaData    Data_Iso
017400080416     c                   adddur    1:*d          Data_Iso
017500080416     c                   clear                   d1dDal
017600080416     c     *dmy          move      Data_Iso      d1dDal
017700080416     c                   endIF
017800080416     c                   eval      d1Dal = d1dDal
017900080416     c                   endIF
018000080416      * --------------
018100040909      * controllo immissione delle date
018200040909     C                   Z-ADD     d1DDAL        G02DAT
018300040909     C                   MOVEL     *BLANK        G02ERR
018400040909     C                   CALL      'XSRDA8'
018500040909     C                   PARM                    WLBDAT
018600040909     C     G02ERR        IFEQ      '1'
018700040909     C                   SETON                                        44  90
018800040909     C                   GOTO      endct1
018900040909     C                   ENDIF
019000040909     C                   Z-ADD     G02INV        DalAmg            8 0
019100040909     C                   Z-ADD     G02dat        D1ddal
019200040909      *
019300040909      * Controllo data finale
019400040909     C     d1Dal         ifeq      0
019500040909     C                   Z-ADD     d1DDal        d1Dal
019600040909     C                   End
019700040909      *
019800040909     C                   Z-ADD     d1Dal         G02DAT
019900040909     C                   MOVEL     *BLANK        G02ERR
020000040909     C                   CALL      'XSRDA8'
020100040909     C                   PARM                    WLBDAT
020200040909     C     G02ERR        IFEQ      '1'
020300040909     C                   SETON                                        42  90
020400040909     C                   GOTO      endct1
020500040909     C                   ENDIF
020600040909     c*
020700040909     C                   Z-ADD     G02INV        AlAmG             8 0
020800040909     C                   Z-ADD     G02dat        d1dal
020900040909     c*
021000040909     C     DalAmg        ifgt      AlAmg
021100080416     c                   eval      alAmg = DalAmg
021200080416     c                   eval      d1dal = D1Ddal
021300080416     C*****              SETON                                        4590
021400080416     C*****              GOTO      endct1
021500040909     C                   END
021600040909      *
021700040909     C     endct1        ENDSR
021800040909      *--------------------------------------------------------------*
021900041013     C     Lancio        begSR
022000041013      *
022100041013     c                   clear                   pardate
022200041013      *
022300041013      * compone il codice numerico
022400041013     c                   move      d1ntw         Tpntw
022500041013     c                   z-add     DalAmg        DAdata
022600041013     c                   z-add     AlAmg         Adata
022700041013      **
022800041103     c                   clear                   kpjbu
022900041103     c                   movel     pardate       kpjbu
023000041103     c                   move      'EU78'        kcoaz
023100150316     c                   if        d1ntw = 'DPD'
023200150316     c                   move      'EU7D'        kcoaz
023300150316     c                   end
023400070619     c*******            move      'K214EDI'     kqebi
023500041013      *
023600041103      * Interattiva
023700041103     c                   if        d1tip = 'I'
023800150316     c                   if        d1ntw = 'DPD'
023900150316     c                   call      'FIEU78R0'
024000150316     c                   parm                    kpjba
024100150316     c                   else
024200041103     c                   call      'FIEU78R'
024300041103     c                   parm                    kpjba
024400150316     c                   end
024500041103      * Batch
024600041103     c                   else
024700091221     c                   if        %subst(Knmus:1:3) ='EDP'
024800041103     c                   call      'BCH09'
024900041103     c                   parm                    kpjba
025000080416     c                   end
025100041103     c                   call      'BCH10'
025200041103     c                   parm                    kpjba
025300041103     c                   end
025400041013      *
025500080416      * Aggiorna il file che tiene conto delle estrazioni effettuate
025600080416     c     d1ntw         chain     tnecx00f
025700080416     c                   eval      ECX_data_dal = DalAmg
025800080416     c                   eval      ECX_data_al  = AlAmg
025900080416     c                   eval      ECX_fatto_il  = *date
026000080416     c                   if        %Found(tnecx00f)
026100080416     c                   update    tnecx000
026200080416     c                   eLSe
026300080416     c                   eval      ECX_network = d1ntw
026400080416     c                   write     tnecx000
026500080416     c                   end
026600041013      *
026700041013     C                   ENDSR
026800041015      *--------------------------------------------------------------*
026900041015      *
027000041013      *--------------------------------------------------------------*
027100041015     C     CTR_tab_EVA   begSR
027200041015      *
027300041015     c                   clear                   kpjbu
027400041015     c                   clear                   tab_noOK          1
027500041015      *
027600041015     c                   movel     'C'           kpjbu
027700041015     c                   call      'FIEU90R'
027800041015     c                   parm                    kpjba
027900041015     c                   movel     kpjbu         tab_noOK
028000041015      *
028100041015     c                   if        tab_noOK = 'S'
028200041015      *
028300041015     c                   end
028400041015      *
028500041015     C                   ENDSR
028600041015      *--------------------------------------------------------------*
028700080416      *  Controlla l'ultima estrazione
028800080416      *--------------------------------------------------------------*
028900080416     C     ctrl_estratto begSR
029000080416      *
029100080416     c     d1ntw         chain     tnecx00f
029200080416     C                   Clear                   UltimaData_x
029300080416     c                   if        %Found(tnecx00f)
029400080416     C                   Z-ADD     ECX_data_al   UltimaData_x
029500080416     c                   end
029600080416      *
029700080416     c                   if        UltimaData_x > UltimaData
029800080416     c                   eval      UltimaData = UltimaData_x
029900080416     c                   end
030000080416      *
030100080416     C                   ENDSR
030200080416      *--------------------------------------------------------------*
