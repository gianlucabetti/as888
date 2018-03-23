000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300140307      *       prompt di lancio Statistica PDA - ORA STIMATA
000400131009      *
000500000000      *****************************************************************
000600130122     FAZORG01L  IF   E           K DISK
000700120402     Ftabel00f  IF   E           K DISK
000800140314     Ftntbe01l  IF   E           K DISK
000900120330     Ffiapd01L  IF   E           K DISK
001000140307     Ffnlvp16d  CF   E             WORKSTN
001100030116      *****************************************************************
001200030116     d KPJBA         E DS
001300140314     D DVPOORARIS    E DS
001400140314     D Tibs36ds      E DS
001500140307     D fnlvp16ds     e DS                  inz
001600120329     D*-----------
001700120329     D* DS PER TRUL06R - CARICAMENTO £X
001800120329     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001900120329     D  LIN                    1     90  0
002000120329     D                                     DIM(30)
002100120329     D L1              S              3  0 DIM(30)
002200101018     D*-----------
002300000000     D WLBDAT          DS
002400940926     D  G02DAT                 1      8  0
002500940926     D  G02INV                 9     16  0
002600940926     D  G02ERR                17     17
002700940926     D  G02TGI                18     22  0
002800030205      *
002900120521     d azuteds       e ds                  extname(azute00f)
003000120521     d ddatiute      e ds
003100120521     d tibs34ds      e ds                  inz
003200120330     d ds5A2         e ds
003300130122     d og150         e ds
003400100617     d dataiso         s               d   datfmt(*iso)
003500100617     d dataeur         s               d   datfmt(*eur)
003600120330     d datapul         s               d   datfmt(*iso)
003700131007     d utente_EDP      s              1
003800000000      *--------------------------------------------------------------*
003900000000      *             M A I N      L I N E
004000000000      *--------------------------------------------------------------*
004100120329     c                   exsr      srinz
004200000000      * Emetto il formato video
004300120329     C                   do        *hival
004400020308     C                   SETOFF                                       90
004500131008     c     Hin01         comp      '1'                                    01
004600030825     C                   EXFMT     CN23D01
004700120329     C   KC              leave
004800000000      * Eseguo i  controlli sui campi immessi a video
004900000000     C                   EXSR      CONTR
005000120329     C   90              iter
005100000000      * Per errore generico riemetto il formato video
005200020307      * Richiamo programma di stampa
005300020308     c                   if        *inkf
005400140307     c                   clear                   fnlvp16ds
005500030116      * compone il codice numerico
005600140307     c     D1DCD1        mult      10000         f16cda
005700140307     c                   move      d1dcd2        f16cda
005800140307     c     d1dca1        mult      10000         f16ca
005900140307     c                   move      D1DCA2        f16ca
006000140307     c                   z-add     DalAmg        f16dti
006100140307     c                   z-add     AlAmg         f16dtf
006200140307     c                   z-add     d1PO          f16PO
006300140307     c                   move      d1PrcMes      f16PrcMes
006400140307     c                   move      D1emiss       f16stafil
006500140307     c                   move      D1test        f16test
006600140311     c                   move      D1scom        f16scom
006700140311     c                   move      D1scop        f16scop
006800140320     c                   z-add     d1primac      f16primac
006900140320     c                   z-add     d1oraokc      f16oraokc
007000140307     c                   movel(p)  fnlvp16ds     kpjbu
007100140307     c                   move      'LVP7'        kcoaz
007200030116      * Lancio Batch
007300030825     c                   if        knmus = *all'1'
007400140307     c                   call      'FNLVP17C'
007500030124     c                   parm                    kpjba
007600030124     c                   else
007700020626     c                   call      'BCH10'
007800020624     c                   parm                    kpjba
007900030124     c                   end
008000120329     c                   exsr      srinz
008100020308     c                   endif
008200000000      *
008300120329     C                   enddo
008400030116      *
008500120329     C                   SETON                                        LR
008600120329      *--------------------------------------------------------------*
008700120329      * Routine per inizializzare i campi
008800120329      *--------------------------------------------------------------*
008900120329     c     srinz         begsr
009000140310     C                   MOVEL     dutPOU        d1PO
009100120329     C                   Z-ADD     d1PO          D1DCD1
009200120329     C                   Z-ADD     d1PO          d1dca1
009300120329      * Inizializzo i campi a video
009400120329     C                   move      *ZERO         d1dcd2
009500120329     C                   move      *All'9'       D1DCA2
009600120329     C                   Z-ADD     *ZERO         D1DDAL
009700120329     C                   Z-ADD     *ZERO         D1DAL
009800140304     C                   move      'N'           d1test
009900120619      * In FILIALE sempre la STAMPA
010000120619     C  N14              move      'S'           d1emiss
010100120619      * In  SEDE   propone il File di estrazione
010200120619     C   14              move      'F'           d1emiss
010300130122     C*-------
010400130122     C* Aggancia Organigramma x picking su messaggerie
010500130122     c                   clear                   og150
010600130122     c                   setoff                                       08
010700140312     c                   if        d1po <> 999
010800130122     C     d1PO          chain     AZORG01l
010900130122    1C                   IF        %found(azorg01l)
011000130122     c                   movel     orgdf0        og150
011100130122     c* p.o. con piking messaggerie
011200130122     c                   if        §ogpkm='S'
011300130122     c                   seton                                        08
011400130122     c                   endif
011500130122     c                   end
011600140312     c                   end
011700130122      *
011800130122      * sempre impostato su Tutto
011900130122     C                   move      'T'           d1prcmes
012000140320     c                   clear                   d1primac
012100140320     c                   clear                   d1oraokc
012200140311     C                   eval      d1scom= 30
012300140311     C                   eval      d1scop= 120
012400140314     c     ktbe          chain     tntbe01l
012500140314     c                   if        %found(tntbe01l) and tbeatb = ' '
012600140314     c                   movel     tbeuni        DVPOORARIS
012700140319     C                   eval      d1scom= §VPORST_Ms
012800140319     C                   eval      d1scop= §VPORST_Ps
012900140314     c                   end
013000130122     C*-------
013100120329     c                   endsr
013200000000      *--------------------------------------------------------------*
013300000000      * Routine per i controlli sui campi del formato video          *
013400000000      *--------------------------------------------------------------*
013500000000     C     CONTR         BEGSR
013600000000      *
013700140307     c                   movea     '000000000000'*in(39)
013800030116     c                   clear                   d1ccd1
013900030116     c                   clear                   d1ccd2
014000140312     c                   if        d1po <> 999
014100120330     C                   Z-ADD     d1PO          D1DCD1
014200120330     C                   Z-ADD     d1PO          d1dca1
014300140312     c                   else
014400140312     C                   Z-ADD     1             D1DCD1
014500140312     C                   Z-ADD     d1PO          d1dca1
014600140312     c                   end
014700030116      * --------------
014800131009     c                   if        utente_EDP = 'N'
014900131008     c****
015000140310     C                   IF        dutPOU <> 46
015100101018      * Controllo L'utente è abilitato alla Filiale
015200101018     c     d1po          lookup    LIN                                    21
015300101018     c                   if        not *in21
015400101018     c                   seton                                        5090
015500120329     C                   leavesr
015600101018     C                   END
015700101022     C                   END
015800131008     c****
015900131008     C                   END
016000130122     C*-------
016100130122     C* Aggancia Organigramma x picking su messaggerie
016200130122     c                   clear                   og150
016300130122     c                   setoff                                       08
016400140312     c                   if        d1po <> 999
016500130122     C     d1PO          chain     AZORG01l
016600130122    1C                   IF        %found(azorg01l)
016700130122     c                   movel     orgdf0        og150
016800130122     c* p.o. con piking messaggerie
016900130122     c                   if        §ogpkm='S'
017000130122     c                   seton                                        08
017100130122     c                   endif
017200130122     c                   end
017300140312     c                   end
017400130122     C  N08              move      'T'           d1prcmes
017500101018      * --------------
017600101018      * Controllo range
017700030117     c     D1DCD1        mult      10000         fld701            7 0
017800030120     c                   move      d1dcd2        fld701
017900030116      *
018000030120     c     d1dca1        mult      10000         fld702            7 0
018100030117     c                   move      D1DCA2        fld702
018200030116      *
018300030116     C     fld702        iflt      fld701
018400030116     C                   SETON                                        4990
018500120329     C                   leavesr
018600030116     C                   END
018700030116      * --------------
018800030825      * Decodifiche Aut
018900131008      *
019000030825     c                   if        D1DCD2 <> 0
019100030116     C                   move      fld701        kpdr              7 0
019200030116     C     Kapd          chain     fiapd01l
019300030116     c                   if        not %Found(fiapd01l)
019400030116     C                   SETON                                        4790
019500120329     C                   leavesr
019600030116     C                   Else
019700100223     C                   MOVEL     apdrsf        d1ccd1
019800030116     C                   End
019900030120     C                   EndIf
020000030116      *
020100030825     c                   if        D1DCA2 <> 9999
020200030116     C                   move      fld702        kpdr
020300030116     C     Kapd          chain     fiapd01l
020400030116     c                   if        not %Found(fiapd01l)
020500030116     C                   SETON                                        4890
020600120329     C                   leavesr
020700030116     C                   Else
020800100223     C                   MOVEL     apdrsf        d1ccd2
020900030116     C                   End
021000030116     C                   EndIf
021100131008      *
021200030116      * --------------
021300020624      * controllo immissione delle date
021400030116     C     d1DDAL        ifeq      0
021500020308     C                   SETON                                        4390
021600120329     C                   leavesr
021700030116     C                   Else
021800030116     C                   Z-ADD     d1DDAL        G02DAT
021900000000     C                   MOVEL     *BLANK        G02ERR
022000940926     C                   CALL      'XSRDA8'
022100000000     C                   PARM                    WLBDAT
022200000000     C     G02ERR        IFEQ      '1'
022300020308     C                   SETON                                        44  90
022400120329     C                   leavesr
022500000000     C                   ENDIF
022600030116     C                   Z-ADD     G02INV        DalAmg            8 0
022700030116     C                   Z-ADD     G02dat        D1ddal
022800020307     c                   endif
022900120330     c* se la data inizio è minore della pulizia errore
023000120330     c                   if        dalamg < datpul
023100120330     C                   SETON                                        46  90
023200120330     C                   leavesr
023300120330     C                   ENDIF
023400030825      *
023500020624      * Controllo data finale
023600030116     C     d1Dal         ifeq      0
023700030127     C                   Z-ADD     d1DDal        d1Dal
023800030127     C                   End
023900030825      *
024000030116     C                   Z-ADD     d1Dal         G02DAT
024100020624     C                   MOVEL     *BLANK        G02ERR
024200020624     C                   CALL      'XSRDA8'
024300020624     C                   PARM                    WLBDAT
024400020624     C     G02ERR        IFEQ      '1'
024500020624     C                   SETON                                        42  90
024600120329     C                   leavesr
024700020624     C                   ENDIF
024800030825     c*
024900030116     C                   Z-ADD     G02INV        AlAmG             8 0
025000030116     C                   Z-ADD     G02dat        d1dal
025100030127     c*
025200030116     C     DalAmg        ifgt      AlAmg
025300020624     C                   SETON                                        4590
025400120329     C                   leavesr
025500020624     C                   END
025600120402     c* se un codice aut, + gg
025700120402     c* se + codici aut, max 1 gg
025800120402     c                   if        fld702 <>fld701 and
025900120402     C                             DalAmg <> AlAmg
026000131007     c                              and
026100140307     c                             utente_EDP = 'N' and
026200140310     c                             dutpou <> 046
026300120402     C                   SETON                                        3990
026400120402     C                   leavesr
026500120402     c                   end
026600140312     c                   if        d1po = 999 and
026700140307     c                             utente_EDP = 'N' and
026800140307     c                             d1emiss = 'S' and
026900140310     c                             dutpou = 046
027000140307     C                   SETON                                        4090
027100140307     C                   leavesr
027200140307     c                   end
027300030129      *
027400120329     C                   ENDSR
027500120329      *****************************************************************
027600120329     c     *inzsr        begsr
027700120329      *****************************************************************
027800120329     C     *ENTRY        PLIST
027900120329     C                   PARM                    KPJBA
028000120330     c*
028100120330     C     Ktab          KLIST
028200120330     C                   KFLD                    tblkut
028300120330     C                   KFLD                    tblcod
028400120330     C                   KFLD                    tblkey
028500120329     c*
028600140314     C     Ktbe          KLIST
028700140314     C                   KFLD                    tbecod
028800140314     C                   KFLD                    tbeke1
028900140314     c                   eval      tbecod = 'VPO'
029000140314     c                   eval      tbeke1 = 'ORARISER'
029100140314     c*
029200120329     C     Kapd          KLIST
029300120330     C                   KFLD                    D1ctip
029400120329     C                   KFLD                    kpdr              7 0
029500120329     C                   move      'A'           D1ctip            1
029600120521     c     *dtaara       define    §azute        azuteds
029700120521     c     *dtaara       define    §datiute      ddatiute
029800120521     c                   in(e)     *dtaara
029900120521     c                   if        %error  or rsut = *blanks
030000120521     c                   clear                   tibs34ds
030100120521     c                   call      'TIBS34R'
030200120521     c                   parm                    tibs34ds
030300120521     c                   in        *dtaara
030400120521     c                   endif
030500120330      *  In base all'utente prende il P.O.
030600120330     C                   CLEAR                   Tibs36Ds
030700120330     c                   EVAL      I36ute = Knmus
030800120330     c                   EVAL      I36Tla = 'L'
030900120330     C                   CALL      'TIBS36R'
031000120330     C                   PARM                    Tibs36Ds
031100120329      *
031200120619     c                   setoff                                       14
031300140310     c                   if        dutpou = 046
031400120619      * se in Sede 14 = *ON
031500120619     c                   seton                                        14
031600120619     c                   end
031700131007      *
031800131007      *   utente EDP può chiedere un range non limitato di selezione
031900131007      *      invece
032000131008      *    gli altri utenti possono chiedere in alternativa
032100131008      *      più giorni per un solo AUT
032200131008      *    oppure
032300131008      *      più AUT per una sola giornata.
032400131007     c                   movel     Knmus         EDPUTE            3
032500131007     c                   eval      utente_EDP = 'N'
032600131008     c                   move      '0'           Hin01             1
032700131007      * controlla se si tratta di utente "EDP"
032800131007     c                   if          EDPUTE ='EDP'
032900131008     c                   move      '1'           Hin01             1
033000131007     c                   eval      utente_EDP = 'S'
033100131007     c                   end
033200120329     C***
033300120329     C* CARICO TABELLA FILIALI GESTITE £1
033400120329     C                   CLEAR                   DSUL06
033500120329     C                   MOVE      '£1'          D06COD
033600140310     C                   MOVEL     dutpou        D06KEY
033700120329     C                   MOVEL     ' '           D06TLA
033800120329     C                   MOVEL     DSUL06        KPJBU
033900120329     C                   CALL      'TRUL06R'
034000120329     C                   PARM                    KPJBA
034100120329     C                   MOVEL     KPJBU         DSUL06
034200120329     C                   MOVEA     LIN           L1
034300131008     C***
034400120330     c* tabella 5a per pulizie
034500120330     c                   eval      tblkut = 1
034600120330     c                   eval      tblcod = '5A'
034700120330     c                   eval      tblkey = '2'
034800120330     c     ktab          chain     tabel00f
034900120330     c                   if        %found(tabel00f)
035000120330     c                   movel     tbluni        ds5a2
035100120330     c                   else
035200120330     c                   clear                   ds5a2
035300120330     c                   end
035400131008     C***
035500120330     C* Imposta la UDATE  da TIME
035600120330     C                   TIME                    W0140            14 0
035700120330     C                   MOVE      W0140         UDATE8            8 0
035800120330     c                   move      udate8        dataeur
035900120330     c                   move      dataeur       dataiso
036000120330     c                   move      dataiso       udate8g           8 0
036100120330     c*
036200120330     c     dataiso       subdur    §5apdal:*d    datapul
036300120330     c                   move      datapul       datpul            8 0
036400120329     c                   endsr
