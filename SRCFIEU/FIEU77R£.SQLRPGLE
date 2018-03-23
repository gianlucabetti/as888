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
006700040909     C                   SETOFF                                       90
006800040909     C                   EXFMT     eu77d01
006900040909      *
007000040909     C   KC              GOTO      FINE                                         F3=USCITA
007100040909      *
007200040909      * Eseguo i  controlli sui campi immessi a video
007300040909     C                   EXSR      CONTR
007400040909      *
007500040909      * Se non vi sono errori ma non e' premuto F6 emetto video
007600040909     C   90
007700040909     CorNkf              GOTO      Video1
007800040909      *
007900040909     C     Video2        TAG
008000040909     C                   SETOFF                                       90
008100040909      *
008200041013      *  Se scelti tutti
008300041013     c                   if        d1ntw = '999'
008400130822     c                   move      'DPD'         d1ntw
008500130822     C                   EXSR      LANCIO
008600130822     c                   move      'FED'         d1ntw
008700130822     C                   EXSR      LANCIO
008800130822     c     *loval        setll     azorg01l
008900130822     c                   read      azorg01l
009000130822     c                   dow       not %Eof(azorg01l)
009100130822     c                   eval      og143 = orgde3
009200130822     c                   if        §OGntw = 'EEX'
009300130822     c                   move      orgfil        d1ntw
009400041013     C                   EXSR      LANCIO
009500130822     c                   end
009600041013     c                   enddo
009700041013      *
009800041013     c                   else
009900041013      *
010000041013      * ESEGUE il caricamento
010100041013     C                   EXSR      LANCIO
010200041013     c                   end
010300041013      *
010400161298      *----------------------------------------------------------------
010500040909     c     FINE          tag
010600030620     c                   seton                                        LR
010700040909      *--------------------------------------------------------------*
010800040909      * Routine per i controlli sui campi del formato video          *
010900040909      *--------------------------------------------------------------*
011000040909     C     CONTR         BEGSR
011100040909      *
011200040909     c                   movea     '0000000000'  *in(41)
011300040909     c                   movea     '0000000000'  *in(51)
011400080416      * --------------
011500080416      * controllo il Network immesso
011600040909     C     d1ntw         ifeq      *blank
011700040909     C                   SETON                                        4190
011800040909     C                   GOTO      endct1
011900080416     c                   else
012000130822      *
012100130822     c                   if        d1ntw <>'DPD' and d1ntw <>'FED'
012200130822      * controlla la LNP se network EDI estero
012300130822     c                   z-add     0             dove              3 0
012400130822     c     digits        check     d1ntw         dove
012500130822     c                   if        dove > 0
012600130822      * ci sono alfanumerici
012700130822     C                   SETON                                        4190
012800130822     C                   GOTO      endct1
012900130822     C                   Else
013000130822     c                   move      d1ntw         ntw_num3          3 0
013100130822     c     ntw_num3      chain     azorg01l
013200130822     c                   if        not %Found(azorg01l)
013300130822     C                   SETON                                        4190
013400130822     C                   GOTO      endct1
013500130822     C                   Else
013600130822     c                   eval      og143 = orgde3
013700130822     c                   if        §ogNTW <> 'EEX'
013800130822     C                   SETON                                        4190
013900130822     C                   GOTO      endct1
014000130822     c                   end
014100130822     c                   end
014200130822     C                   End
014300130822      *
014400130822     C                   End
014500130822     C                   End
014600040909      * --------------
014700080416      * controllo immissione dalla data o la protegge
014800080416      * --------------
014900080416     c                   If        d1NtwSav <> d1Ntw
015000080416     c                   clear                   UltimaData
015100080416     C/EXEC SQL
015200080416     C+ SELECT max(ECFAAS*10000+ecfmgs) INTO :UltimaData
015300080416     C+ FROM tnecf00f WHERE ecfntw= :d1ntw
015400080416     C/END-EXEC
015500080416     c                   setoff                                       28
015600080416     c                   exsr      ctrl_estratto
015700080416      *
015800080416      * Al primo cambio di Network  Pulisce le date e riemette il video
015900080416     c                   if        sqlcod < 0  or UltimaData = 0
016000080416     c                   clear                   d1dDal
016100080416     c                   clear                   d1Dal
016200080416     c                   goto      endct1
016300080416     c                   end
016400080416      *
016500080416      * protegge se presenti già dei dati sul Network
016600080416     c                   if        UltimaData > 0
016700080416     c     *iso          move      UltimaData    Data_Iso
016800080416     c                   adddur    1:*d          Data_Iso
016900080416     c                   clear                   d1dDal
017000080416     c     *dmy          move      Data_Iso      d1dDal
017100080416     c                   endIF
017200080416     c                   eval      d1Dal = d1dDal
017300080416     c                   endIF
017400080416      * --------------
017500040909      * controllo immissione delle date
017600040909     C                   Z-ADD     d1DDAL        G02DAT
017700040909     C                   MOVEL     *BLANK        G02ERR
017800040909     C                   CALL      'XSRDA8'
017900040909     C                   PARM                    WLBDAT
018000040909     C     G02ERR        IFEQ      '1'
018100040909     C                   SETON                                        44  90
018200040909     C                   GOTO      endct1
018300040909     C                   ENDIF
018400040909     C                   Z-ADD     G02INV        DalAmg            8 0
018500040909     C                   Z-ADD     G02dat        D1ddal
018600040909      *
018700040909      * Controllo data finale
018800040909     C     d1Dal         ifeq      0
018900040909     C                   Z-ADD     d1DDal        d1Dal
019000040909     C                   End
019100040909      *
019200040909     C                   Z-ADD     d1Dal         G02DAT
019300040909     C                   MOVEL     *BLANK        G02ERR
019400040909     C                   CALL      'XSRDA8'
019500040909     C                   PARM                    WLBDAT
019600040909     C     G02ERR        IFEQ      '1'
019700040909     C                   SETON                                        42  90
019800040909     C                   GOTO      endct1
019900040909     C                   ENDIF
020000040909     c*
020100040909     C                   Z-ADD     G02INV        AlAmG             8 0
020200040909     C                   Z-ADD     G02dat        d1dal
020300040909     c*
020400040909     C     DalAmg        ifgt      AlAmg
020500080416     c                   eval      alAmg = DalAmg
020600080416     c                   eval      d1dal = D1Ddal
020700080416     C*****              SETON                                        4590
020800080416     C*****              GOTO      endct1
020900040909     C                   END
021000040909      *
021100040909     C     endct1        ENDSR
021200040909      *--------------------------------------------------------------*
021300041013     C     Lancio        begSR
021400041013      *
021500041013     c                   clear                   pardate
021600041013      *
021700041013      * compone il codice numerico
021800041013     c                   move      d1ntw         Tpntw
021900041013     c                   z-add     DalAmg        DAdata
022000041013     c                   z-add     AlAmg         Adata
022100041013      **
022200041103     c                   clear                   kpjbu
022300041103     c                   movel     pardate       kpjbu
022400041103     c                   move      'EU78'        kcoaz
022500070619     c*******            move      'K214EDI'     kqebi
022600041013      *
022700041103      * Interattiva
022800041103     c                   if        d1tip = 'I'
022900041103     c                   call      'FIEU78R'
023000041103     c                   parm                    kpjba
023100041103      * Batch
023200041103     c                   else
023300091221     c                   if        %subst(Knmus:1:3) ='EDP'
023400041103     c                   call      'BCH09'
023500041103     c                   parm                    kpjba
023600080416     c                   end
023700041103     c                   call      'BCH10'
023800041103     c                   parm                    kpjba
023900041103     c                   end
024000041013      *
024100080416      * Aggiorna il file che tiene conto delle estrazioni effettuate
024200080416     c     d1ntw         chain     tnecx00f
024300080416     c                   eval      ECX_data_dal = DalAmg
024400080416     c                   eval      ECX_data_al  = AlAmg
024500080416     c                   eval      ECX_fatto_il  = *date
024600080416     c                   if        %Found(tnecx00f)
024700080416     c                   update    tnecx000
024800080416     c                   eLSe
024900080416     c                   eval      ECX_network = d1ntw
025000080416     c                   write     tnecx000
025100080416     c                   end
025200041013      *
025300041013     C                   ENDSR
025400041015      *--------------------------------------------------------------*
025500041015      *
025600041013      *--------------------------------------------------------------*
025700041015     C     CTR_tab_EVA   begSR
025800041015      *
025900041015     c                   clear                   kpjbu
026000041015     c                   clear                   tab_noOK          1
026100041015      *
026200041015     c                   movel     'C'           kpjbu
026300041015     c                   call      'FIEU90R'
026400041015     c                   parm                    kpjba
026500041015     c                   movel     kpjbu         tab_noOK
026600041015      *
026700041015     c                   if        tab_noOK = 'S'
026800041015      *
026900041015     c                   end
027000041015      *
027100041015     C                   ENDSR
027200041015      *--------------------------------------------------------------*
027300080416      *  Controlla l'ultima estrazione
027400080416      *--------------------------------------------------------------*
027500080416     C     ctrl_estratto begSR
027600080416      *
027700080416     c     d1ntw         chain     tnecx00f
027800080416     C                   Clear                   UltimaData_x
027900080416     c                   if        %Found(tnecx00f)
028000080416     C                   Z-ADD     ECX_data_al   UltimaData_x
028100080416     c                   end
028200080416      *
028300080416     c                   if        UltimaData_x > UltimaData
028400080416     c                   eval      UltimaData = UltimaData_x
028500080416     c                   end
028600080416      *
028700080416     C                   ENDSR
028800080416      *--------------------------------------------------------------*
