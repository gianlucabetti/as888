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
005300140428     c                   if        *inkf or *inkv
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
007000140429     c                   clear                   kqieb
007100140429     c                   clear                   kqebi
007200140307     c                   movel(p)  fnlvp16ds     kpjbu
007300140307     c                   move      'LVP7'        kcoaz
007400030116      * Lancio Batch
007500140428     c                   if        *inkv
007600140428     C                   call      'BCH09'
007700140428     C                   parm                    KPJBA
007800140428     c                   endif
007900030825     c                   if        knmus = *all'1'
008000140307     c                   call      'FNLVP17C'
008100030124     c                   parm                    kpjba
008200030124     c                   else
008300020626     c                   call      'BCH10'
008400020624     c                   parm                    kpjba
008500030124     c                   end
008600120329     c                   exsr      srinz
008700020308     c                   endif
008800000000      *
008900120329     C                   enddo
009000030116      *
009100120329     C                   SETON                                        LR
009200120329      *--------------------------------------------------------------*
009300120329      * Routine per inizializzare i campi
009400120329      *--------------------------------------------------------------*
009500120329     c     srinz         begsr
009600140310     C                   MOVEL     dutPOU        d1PO
009700120329     C                   Z-ADD     d1PO          D1DCD1
009800120329     C                   Z-ADD     d1PO          d1dca1
009900120329      * Inizializzo i campi a video
010000120329     C                   move      *ZERO         d1dcd2
010100120329     C                   move      *All'9'       D1DCA2
010200120329     C                   Z-ADD     *ZERO         D1DDAL
010300120329     C                   Z-ADD     *ZERO         D1DAL
010400140304     C                   move      'N'           d1test
010500120619      * In FILIALE sempre la STAMPA
010600120619     C  N14              move      'S'           d1emiss
010700120619      * In  SEDE   propone il File di estrazione
010800120619     C   14              move      'F'           d1emiss
010900130122     C*-------
011000130122     C* Aggancia Organigramma x picking su messaggerie
011100130122     c                   clear                   og150
011200130122     c                   setoff                                       08
011300140312     c                   if        d1po <> 999
011400130122     C     d1PO          chain     AZORG01l
011500130122    1C                   IF        %found(azorg01l)
011600130122     c                   movel     orgdf0        og150
011700130122     c* p.o. con piking messaggerie
011800130122     c                   if        §ogpkm='S'
011900130122     c                   seton                                        08
012000130122     c                   endif
012100130122     c                   end
012200140312     c                   end
012300130122      *
012400130122      * sempre impostato su Tutto
012500130122     C                   move      'T'           d1prcmes
012600140320     c                   clear                   d1primac
012700140320     c                   clear                   d1oraokc
012800140311     C                   eval      d1scom= 30
012900140311     C                   eval      d1scop= 120
013000140314     c     ktbe          chain     tntbe01l
013100140314     c                   if        %found(tntbe01l) and tbeatb = ' '
013200140314     c                   movel     tbeuni        DVPOORARIS
013300140319     C                   eval      d1scom= §VPORST_Ms
013400140319     C                   eval      d1scop= §VPORST_Ps
013500140314     c                   end
013600130122     C*-------
013700120329     c                   endsr
013800000000      *--------------------------------------------------------------*
013900000000      * Routine per i controlli sui campi del formato video          *
014000000000      *--------------------------------------------------------------*
014100000000     C     CONTR         BEGSR
014200000000      *
014300140307     c                   movea     '000000000000'*in(39)
014400030116     c                   clear                   d1ccd1
014500030116     c                   clear                   d1ccd2
014600140312     c                   if        d1po <> 999
014700120330     C                   Z-ADD     d1PO          D1DCD1
014800120330     C                   Z-ADD     d1PO          d1dca1
014900140312     c                   else
015000140312     C                   Z-ADD     1             D1DCD1
015100140312     C                   Z-ADD     d1PO          d1dca1
015200140312     c                   end
015300030116      * --------------
015400131009     c                   if        utente_EDP = 'N'
015500131008     c****
015600140310     C                   IF        dutPOU <> 46
015700101018      * Controllo L'utente è abilitato alla Filiale
015800101018     c     d1po          lookup    LIN                                    21
015900101018     c                   if        not *in21
016000101018     c                   seton                                        5090
016100120329     C                   leavesr
016200101018     C                   END
016300101022     C                   END
016400131008     c****
016500131008     C                   END
016600130122     C*-------
016700130122     C* Aggancia Organigramma x picking su messaggerie
016800130122     c                   clear                   og150
016900130122     c                   setoff                                       08
017000140312     c                   if        d1po <> 999
017100130122     C     d1PO          chain     AZORG01l
017200130122    1C                   IF        %found(azorg01l)
017300130122     c                   movel     orgdf0        og150
017400130122     c* p.o. con piking messaggerie
017500130122     c                   if        §ogpkm='S'
017600130122     c                   seton                                        08
017700130122     c                   endif
017800130122     c                   end
017900140312     c                   end
018000130122     C  N08              move      'T'           d1prcmes
018100101018      * --------------
018200101018      * Controllo range
018300030117     c     D1DCD1        mult      10000         fld701            7 0
018400030120     c                   move      d1dcd2        fld701
018500030116      *
018600030120     c     d1dca1        mult      10000         fld702            7 0
018700030117     c                   move      D1DCA2        fld702
018800030116      *
018900030116     C     fld702        iflt      fld701
019000030116     C                   SETON                                        4990
019100120329     C                   leavesr
019200030116     C                   END
019300030116      * --------------
019400030825      * Decodifiche Aut
019500131008      *
019600030825     c                   if        D1DCD2 <> 0
019700030116     C                   move      fld701        kpdr              7 0
019800030116     C     Kapd          chain     fiapd01l
019900030116     c                   if        not %Found(fiapd01l)
020000030116     C                   SETON                                        4790
020100120329     C                   leavesr
020200030116     C                   Else
020300100223     C                   MOVEL     apdrsf        d1ccd1
020400030116     C                   End
020500030120     C                   EndIf
020600030116      *
020700030825     c                   if        D1DCA2 <> 9999
020800030116     C                   move      fld702        kpdr
020900030116     C     Kapd          chain     fiapd01l
021000030116     c                   if        not %Found(fiapd01l)
021100030116     C                   SETON                                        4890
021200120329     C                   leavesr
021300030116     C                   Else
021400100223     C                   MOVEL     apdrsf        d1ccd2
021500030116     C                   End
021600030116     C                   EndIf
021700131008      *
021800030116      * --------------
021900020624      * controllo immissione delle date
022000030116     C     d1DDAL        ifeq      0
022100020308     C                   SETON                                        4390
022200120329     C                   leavesr
022300030116     C                   Else
022400030116     C                   Z-ADD     d1DDAL        G02DAT
022500000000     C                   MOVEL     *BLANK        G02ERR
022600940926     C                   CALL      'XSRDA8'
022700000000     C                   PARM                    WLBDAT
022800000000     C     G02ERR        IFEQ      '1'
022900020308     C                   SETON                                        44  90
023000120329     C                   leavesr
023100000000     C                   ENDIF
023200030116     C                   Z-ADD     G02INV        DalAmg            8 0
023300030116     C                   Z-ADD     G02dat        D1ddal
023400020307     c                   endif
023500120330     c* se la data inizio è minore della pulizia errore
023600120330     c                   if        dalamg < datpul
023700120330     C                   SETON                                        46  90
023800120330     C                   leavesr
023900120330     C                   ENDIF
024000030825      *
024100020624      * Controllo data finale
024200030116     C     d1Dal         ifeq      0
024300030127     C                   Z-ADD     d1DDal        d1Dal
024400030127     C                   End
024500030825      *
024600030116     C                   Z-ADD     d1Dal         G02DAT
024700020624     C                   MOVEL     *BLANK        G02ERR
024800020624     C                   CALL      'XSRDA8'
024900020624     C                   PARM                    WLBDAT
025000020624     C     G02ERR        IFEQ      '1'
025100020624     C                   SETON                                        42  90
025200120329     C                   leavesr
025300020624     C                   ENDIF
025400030825     c*
025500030116     C                   Z-ADD     G02INV        AlAmG             8 0
025600030116     C                   Z-ADD     G02dat        d1dal
025700030127     c*
025800030116     C     DalAmg        ifgt      AlAmg
025900020624     C                   SETON                                        4590
026000120329     C                   leavesr
026100020624     C                   END
026200120402     c* se un codice aut, + gg
026300120402     c* se + codici aut, max 1 gg
026400120402     c                   if        fld702 <>fld701 and
026500120402     C                             DalAmg <> AlAmg
026600131007     c                              and
026700140307     c                             utente_EDP = 'N' and
026800140310     c                             dutpou <> 046
026900120402     C                   SETON                                        3990
027000120402     C                   leavesr
027100120402     c                   end
027200140312     c                   if        d1po = 999 and
027300140307     c                             utente_EDP = 'N' and
027400140307     c                             d1emiss = 'S' and
027500140310     c                             dutpou = 046
027600140307     C                   SETON                                        4090
027700140307     C                   leavesr
027800140307     c                   end
027900030129      *
028000120329     C                   ENDSR
028100120329      *****************************************************************
028200120329     c     *inzsr        begsr
028300120329      *****************************************************************
028400120329     C     *ENTRY        PLIST
028500120329     C                   PARM                    KPJBA
028600120330     c*
028700120330     C     Ktab          KLIST
028800120330     C                   KFLD                    tblkut
028900120330     C                   KFLD                    tblcod
029000120330     C                   KFLD                    tblkey
029100120329     c*
029200140314     C     Ktbe          KLIST
029300140314     C                   KFLD                    tbecod
029400140314     C                   KFLD                    tbeke1
029500140314     c                   eval      tbecod = 'VPO'
029600140314     c                   eval      tbeke1 = 'ORARISER'
029700140314     c*
029800120329     C     Kapd          KLIST
029900120330     C                   KFLD                    D1ctip
030000120329     C                   KFLD                    kpdr              7 0
030100120329     C                   move      'A'           D1ctip            1
030200120521     c     *dtaara       define    §azute        azuteds
030300120521     c     *dtaara       define    §datiute      ddatiute
030400120521     c                   in(e)     *dtaara
030500120521     c                   if        %error  or rsut = *blanks
030600120521     c                   clear                   tibs34ds
030700120521     c                   call      'TIBS34R'
030800120521     c                   parm                    tibs34ds
030900120521     c                   in        *dtaara
031000120521     c                   endif
031100120330      *  In base all'utente prende il P.O.
031200120330     C                   CLEAR                   Tibs36Ds
031300120330     c                   EVAL      I36ute = Knmus
031400120330     c                   EVAL      I36Tla = 'L'
031500120330     C                   CALL      'TIBS36R'
031600120330     C                   PARM                    Tibs36Ds
031700120329      *
031800120619     c                   setoff                                       14
031900140310     c                   if        dutpou = 046
032000120619      * se in Sede 14 = *ON
032100120619     c                   seton                                        14
032200120619     c                   end
032300131007      *
032400131007      *   utente EDP può chiedere un range non limitato di selezione
032500131007      *      invece
032600131008      *    gli altri utenti possono chiedere in alternativa
032700131008      *      più giorni per un solo AUT
032800131008      *    oppure
032900131008      *      più AUT per una sola giornata.
033000131007     c                   movel     Knmus         EDPUTE            3
033100131007     c                   eval      utente_EDP = 'N'
033200131008     c                   move      '0'           Hin01             1
033300131007      * controlla se si tratta di utente "EDP"
033400131007     c                   if          EDPUTE ='EDP'
033500131008     c                   move      '1'           Hin01             1
033600131007     c                   eval      utente_EDP = 'S'
033700131007     c                   end
033800120329     C***
033900120329     C* CARICO TABELLA FILIALI GESTITE £1
034000120329     C                   CLEAR                   DSUL06
034100120329     C                   MOVE      '£1'          D06COD
034200140310     C                   MOVEL     dutpou        D06KEY
034300120329     C                   MOVEL     ' '           D06TLA
034400120329     C                   MOVEL     DSUL06        KPJBU
034500120329     C                   CALL      'TRUL06R'
034600120329     C                   PARM                    KPJBA
034700120329     C                   MOVEL     KPJBU         DSUL06
034800120329     C                   MOVEA     LIN           L1
034900131008     C***
035000120330     c* tabella 5a per pulizie
035100120330     c                   eval      tblkut = 1
035200120330     c                   eval      tblcod = '5A'
035300120330     c                   eval      tblkey = '2'
035400120330     c     ktab          chain     tabel00f
035500120330     c                   if        %found(tabel00f)
035600120330     c                   movel     tbluni        ds5a2
035700120330     c                   else
035800120330     c                   clear                   ds5a2
035900120330     c                   end
036000131008     C***
036100120330     C* Imposta la UDATE  da TIME
036200120330     C                   TIME                    W0140            14 0
036300120330     C                   MOVE      W0140         UDATE8            8 0
036400120330     c                   move      udate8        dataeur
036500120330     c                   move      dataeur       dataiso
036600120330     c                   move      dataiso       udate8g           8 0
036700120330     c*
036800120330     c     dataiso       subdur    §5apdal:*d    datapul
036900120330     c                   move      datapul       datpul            8 0
037000120329     c                   endsr
