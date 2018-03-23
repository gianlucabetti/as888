000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030825      *       prompt di lancio stampa saldi di periodo
000400131009      *
000500000000      *****************************************************************
000600130122     FAZORG01L  IF   E           K DISK
000700120402     Ftabel00f  IF   E           K DISK
000800120330     Ffiapd01L  IF   E           K DISK
000900120329     Ffnlvp10d  CF   E             WORKSTN
001000030116      *****************************************************************
001100030116     d KPJBA         E DS
001200120330     D Tibs36ds      E DS
001300140304     D fnlvp10ds     e DS                  inz
001400120329     D*-----------
001500120329     D* DS PER TRUL06R - CARICAMENTO £X
001600120329     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001700120329     D  LIN                    1     90  0
001800120329     D                                     DIM(30)
001900120329     D L1              S              3  0 DIM(30)
002000101018     D*-----------
002100000000     D WLBDAT          DS
002200940926     D  G02DAT                 1      8  0
002300940926     D  G02INV                 9     16  0
002400940926     D  G02ERR                17     17
002500940926     D  G02TGI                18     22  0
002600030205      *
002700120521     d azuteds       e ds                  extname(azute00f)
002800120521     d ddatiute      e ds
002900120521     d tibs34ds      e ds                  inz
003000120330     d ds5A2         e ds
003100130122     d og150         e ds
003200100617     d dataiso         s               d   datfmt(*iso)
003300100617     d dataeur         s               d   datfmt(*eur)
003400120330     d datapul         s               d   datfmt(*iso)
003500131007     d utente_EDP      s              1
003600000000      *--------------------------------------------------------------*
003700000000      *             M A I N      L I N E
003800000000      *--------------------------------------------------------------*
003900120329     c                   exsr      srinz
004000000000      * Emetto il formato video
004100120329     C                   do        *hival
004200020308     C                   SETOFF                                       90
004300131008     c     Hin01         comp      '1'                                    01
004400030825     C                   EXFMT     CN23D01
004500120329     C   KC              leave
004600000000      * Eseguo i  controlli sui campi immessi a video
004700000000     C                   EXSR      CONTR
004800120329     C   90              iter
004900000000      * Per errore generico riemetto il formato video
005000020307      * Richiamo programma di stampa
005100020308     c                   if        *inkf
005200120329     c                   clear                   fnlvp10ds
005300030116      * compone il codice numerico
005400120329     c     D1DCD1        mult      10000         f10cda
005500120329     c                   move      d1dcd2        f10cda
005600120329     c     d1dca1        mult      10000         f10ca
005700120329     c                   move      D1DCA2        f10ca
005800120329     c                   z-add     DalAmg        f10dti
005900120329     c                   z-add     AlAmg         f10dtf
006000120329     c                   z-add     d1PO          f10PO
006100130122     c                   move      d1PrcMes      f10PrcMes
006200120618     c                   move      D1emiss       f10stafil
006201140304     c                   move      D1test        f10test
006300120329     c                   movel(p)  fnlvp10ds     kpjbu
006400120329     c                   move      'LVP1'        kcoaz
006500030116      * Lancio Batch
006600030825     c                   if        knmus = *all'1'
006700120618     c                   call      'FNLVP11C'
006800030124     c                   parm                    kpjba
006900030124     c                   else
007000020626     c                   call      'BCH10'
007100020624     c                   parm                    kpjba
007200030124     c                   end
007300120329     c                   exsr      srinz
007400020308     c                   endif
007500000000      *
007600120329     C                   enddo
007700030116      *
007800120329     C                   SETON                                        LR
007900120329      *--------------------------------------------------------------*
008000120329      * Routine per inizializzare i campi
008100120329      *--------------------------------------------------------------*
008200120329     c     srinz         begsr
008300120330     C                   MOVEL     o36POU        d1PO
008400120329     C                   Z-ADD     d1PO          D1DCD1
008500120329     C                   Z-ADD     d1PO          d1dca1
008600120329      * Inizializzo i campi a video
008700120329     C                   move      *ZERO         d1dcd2
008800120329     C                   move      *All'9'       D1DCA2
008900120329     C                   Z-ADD     *ZERO         D1DDAL
009000120329     C                   Z-ADD     *ZERO         D1DAL
009001140304     C                   move      'N'           d1test
009100120619      * In FILIALE sempre la STAMPA
009200120619     C  N14              move      'S'           d1emiss
009300120619      * In  SEDE   propone il File di estrazione
009400120619     C   14              move      'F'           d1emiss
009500130122     C*-------
009600130122     C* Aggancia Organigramma x picking su messaggerie
009700130122     c                   clear                   og150
009800130122     c                   setoff                                       08
009900130122     C     d1PO          chain     AZORG01l
010000130122    1C                   IF        %found(azorg01l)
010100130122     c                   movel     orgdf0        og150
010200130122     c* p.o. con piking messaggerie
010300130122     c                   if        §ogpkm='S'
010400130122     c                   seton                                        08
010500130122     c                   endif
010600130122     c                   end
010700130122      *
010800130122      * sempre impostato su Tutto
010900130122     C                   move      'T'           d1prcmes
011000130122     C*-------
011100120329     c                   endsr
011200000000      *--------------------------------------------------------------*
011300000000      * Routine per i controlli sui campi del formato video          *
011400000000      *--------------------------------------------------------------*
011500000000     C     CONTR         BEGSR
011600000000      *
011700030116     c                   movea     '0000000000'  *in(41)
011800030116     c                   clear                   d1ccd1
011900030116     c                   clear                   d1ccd2
012000120330     C                   Z-ADD     d1PO          D1DCD1
012100120330     C                   Z-ADD     d1PO          d1dca1
012200030116      * --------------
012300131009     c                   if        utente_EDP = 'N'
012400131008     c****
012500120330     C                   IF        O36POU <> 46
012600101018      * Controllo L'utente è abilitato alla Filiale
012700101018     c     d1po          lookup    LIN                                    21
012800101018     c                   if        not *in21
012900101018     c                   seton                                        5090
013000120329     C                   leavesr
013100101018     C                   END
013200101022     C                   END
013300131008     c****
013400131008     C                   END
013500130122     C*-------
013600130122     C* Aggancia Organigramma x picking su messaggerie
013700130122     c                   clear                   og150
013800130122     c                   setoff                                       08
013900130122     C     d1PO          chain     AZORG01l
014000130122    1C                   IF        %found(azorg01l)
014100130122     c                   movel     orgdf0        og150
014200130122     c* p.o. con piking messaggerie
014300130122     c                   if        §ogpkm='S'
014400130122     c                   seton                                        08
014500130122     c                   endif
014600130122     c                   end
014700130122     C  N08              move      'T'           d1prcmes
014800101018      * --------------
014900101018      * Controllo range
015000030117     c     D1DCD1        mult      10000         fld701            7 0
015100030120     c                   move      d1dcd2        fld701
015200030116      *
015300030120     c     d1dca1        mult      10000         fld702            7 0
015400030117     c                   move      D1DCA2        fld702
015500030116      *
015600030116     C     fld702        iflt      fld701
015700030116     C                   SETON                                        4990
015800120329     C                   leavesr
015900030116     C                   END
016000030116      * --------------
016100030825      * Decodifiche Aut
016200131008      *
016300030825     c                   if        D1DCD2 <> 0
016400030116     C                   move      fld701        kpdr              7 0
016500030116     C     Kapd          chain     fiapd01l
016600030116     c                   if        not %Found(fiapd01l)
016700030116     C                   SETON                                        4790
016800120329     C                   leavesr
016900030116     C                   Else
017000100223     C                   MOVEL     apdrsf        d1ccd1
017100030116     C                   End
017200030120     C                   EndIf
017300030116      *
017400030825     c                   if        D1DCA2 <> 9999
017500030116     C                   move      fld702        kpdr
017600030116     C     Kapd          chain     fiapd01l
017700030116     c                   if        not %Found(fiapd01l)
017800030116     C                   SETON                                        4890
017900120329     C                   leavesr
018000030116     C                   Else
018100100223     C                   MOVEL     apdrsf        d1ccd2
018200030116     C                   End
018300030116     C                   EndIf
018400131008      *
018500030116      * --------------
018600020624      * controllo immissione delle date
018700030116     C     d1DDAL        ifeq      0
018800020308     C                   SETON                                        4390
018900120329     C                   leavesr
019000030116     C                   Else
019100030116     C                   Z-ADD     d1DDAL        G02DAT
019200000000     C                   MOVEL     *BLANK        G02ERR
019300940926     C                   CALL      'XSRDA8'
019400000000     C                   PARM                    WLBDAT
019500000000     C     G02ERR        IFEQ      '1'
019600020308     C                   SETON                                        44  90
019700120329     C                   leavesr
019800000000     C                   ENDIF
019900030116     C                   Z-ADD     G02INV        DalAmg            8 0
020000030116     C                   Z-ADD     G02dat        D1ddal
020100020307     c                   endif
020200120330     c* se la data inizio è minore della pulizia errore
020300120330     c                   if        dalamg < datpul
020400120330     C                   SETON                                        46  90
020500120330     C                   leavesr
020600120330     C                   ENDIF
020700030825      *
020800020624      * Controllo data finale
020900030116     C     d1Dal         ifeq      0
021000030127     C                   Z-ADD     d1DDal        d1Dal
021100030127     C                   End
021200030825      *
021300030116     C                   Z-ADD     d1Dal         G02DAT
021400020624     C                   MOVEL     *BLANK        G02ERR
021500020624     C                   CALL      'XSRDA8'
021600020624     C                   PARM                    WLBDAT
021700020624     C     G02ERR        IFEQ      '1'
021800020624     C                   SETON                                        42  90
021900120329     C                   leavesr
022000020624     C                   ENDIF
022100030825     c*
022200030116     C                   Z-ADD     G02INV        AlAmG             8 0
022300030116     C                   Z-ADD     G02dat        d1dal
022400030127     c*
022500030116     C     DalAmg        ifgt      AlAmg
022600020624     C                   SETON                                        4590
022700120329     C                   leavesr
022800020624     C                   END
022900120402     c* se un codice aut, + gg
023000120402     c* se + codici aut, max 1 gg
023100120402     c                   if        fld702 <>fld701 and
023200120402     C                             DalAmg <> AlAmg
023300131007     c                              and
023400131007     c                             utente_EDP = 'N'
023500120402     C                   SETON                                        3990
023600120402     C                   leavesr
023700120402     c                   end
023800030129      *
023900120329     C                   ENDSR
024000120329      *****************************************************************
024100120329     c     *inzsr        begsr
024200120329      *****************************************************************
024300120329     C     *ENTRY        PLIST
024400120329     C                   PARM                    KPJBA
024500120330     c*
024600120330     C     Ktab          KLIST
024700120330     C                   KFLD                    tblkut
024800120330     C                   KFLD                    tblcod
024900120330     C                   KFLD                    tblkey
025000120329     c*
025100120329     C     Kapd          KLIST
025200120330     C                   KFLD                    D1ctip
025300120329     C                   KFLD                    kpdr              7 0
025400120329     C                   move      'A'           D1ctip            1
025500120521     c     *dtaara       define    §azute        azuteds
025600120521     c     *dtaara       define    §datiute      ddatiute
025700120521     c                   in(e)     *dtaara
025800120521     c                   if        %error  or rsut = *blanks
025900120521     c                   clear                   tibs34ds
026000120521     c                   call      'TIBS34R'
026100120521     c                   parm                    tibs34ds
026200120521     c                   in        *dtaara
026300120521     c                   endif
026400120330      *  In base all'utente prende il P.O.
026500120330     C                   CLEAR                   Tibs36Ds
026600120330     c                   EVAL      I36ute = Knmus
026700120330     c                   EVAL      I36Tla = 'L'
026800120330     C                   CALL      'TIBS36R'
026900120330     C                   PARM                    Tibs36Ds
027000120329      *
027100120619     c                   setoff                                       14
027200120619     c                   if        o36pou = 046
027300120619      * se in Sede 14 = *ON
027400120619     c                   seton                                        14
027500120619     c                   end
027600131007      *
027700131007      *   utente EDP può chiedere un range non limitato di selezione
027800131007      *      invece
027900131008      *    gli altri utenti possono chiedere in alternativa
028000131008      *      più giorni per un solo AUT
028100131008      *    oppure
028200131008      *      più AUT per una sola giornata.
028300131007     c                   movel     Knmus         EDPUTE            3
028400131007     c                   eval      utente_EDP = 'N'
028500131008     c                   move      '0'           Hin01             1
028600131007      * controlla se si tratta di utente "EDP"
028700131007     c                   if          EDPUTE ='EDP'
028800131008     c                   move      '1'           Hin01             1
028900131007     c                   eval      utente_EDP = 'S'
029000131007     c                   end
029100120329     C***
029200120329     C* CARICO TABELLA FILIALI GESTITE £1
029300120329     C                   CLEAR                   DSUL06
029400120329     C                   MOVE      '£1'          D06COD
029500120330     C                   MOVEL     o36pou        D06KEY
029600120329     C                   MOVEL     ' '           D06TLA
029700120329     C                   MOVEL     DSUL06        KPJBU
029800120329     C                   CALL      'TRUL06R'
029900120329     C                   PARM                    KPJBA
030000120329     C                   MOVEL     KPJBU         DSUL06
030100120329     C                   MOVEA     LIN           L1
030200131008     C***
030300120330     c* tabella 5a per pulizie
030400120330     c                   eval      tblkut = 1
030500120330     c                   eval      tblcod = '5A'
030600120330     c                   eval      tblkey = '2'
030700120330     c     ktab          chain     tabel00f
030800120330     c                   if        %found(tabel00f)
030900120330     c                   movel     tbluni        ds5a2
031000120330     c                   else
031100120330     c                   clear                   ds5a2
031200120330     c                   end
031300131008     C***
031400120330     C* Imposta la UDATE  da TIME
031500120330     C                   TIME                    W0140            14 0
031600120330     C                   MOVE      W0140         UDATE8            8 0
031700120330     c                   move      udate8        dataeur
031800120330     c                   move      dataeur       dataiso
031900120330     c                   move      dataiso       udate8g           8 0
032000120330     c*
032100120330     c     dataiso       subdur    §5apdal:*d    datapul
032200120330     c                   move      datapul       datpul            8 0
032300120329     c                   endsr
