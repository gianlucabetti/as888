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
001600170327
001700170327      // - Reperimento filiali gestite dall'utente
001800170327     d TRUL31DS      e ds
001900170327     d  skpoel                10    759    dim(250)
002000170327
002100120329     D*-----------
002200000000     D WLBDAT          DS
002300940926     D  G02DAT                 1      8  0
002400940926     D  G02INV                 9     16  0
002500940926     D  G02ERR                17     17
002600940926     D  G02TGI                18     22  0
002700030205      *
002800120521     d azuteds       e ds                  extname(azute00f)
002900120521     d ddatiute      e ds
003000120521     d tibs34ds      e ds                  inz
003100120330     d ds5A2         e ds
003200130122     d og150         e ds
003300100617     d dataiso         s               d   datfmt(*iso)
003400100617     d dataeur         s               d   datfmt(*eur)
003500120330     d datapul         s               d   datfmt(*iso)
003600131007     d utente_EDP      s              1
003700170327
003800170327      //---------------------------------------------------------------
003900170327      //?Definizione Prototipi.
004000170327      //---------------------------------------------------------------
004100170327      // - Caricamento Filiali in gestione
004200170327     d TRUL31R         pr                  extpgm('TRUL31R')
004300170327     d  kpjba                              likeds(kpjba)
004400170327     d  trul31ds                           likeds(trul31ds)
004500000000      *--------------------------------------------------------------*
004600000000      *             M A I N      L I N E
004700000000      *--------------------------------------------------------------*
004800120329     c                   exsr      srinz
004900000000      * Emetto il formato video
005000120329     C                   do        *hival
005100020308     C                   SETOFF                                       90
005200131008     c     Hin01         comp      '1'                                    01
005300030825     C                   EXFMT     CN23D01
005400120329     C   KC              leave
005500000000      * Eseguo i  controlli sui campi immessi a video
005600000000     C                   EXSR      CONTR
005700120329     C   90              iter
005800000000      * Per errore generico riemetto il formato video
005900020307      * Richiamo programma di stampa
006000140428     c                   if        *inkf or *inkv
006100140307     c                   clear                   fnlvp16ds
006200030116      * compone il codice numerico
006300140307     c     D1DCD1        mult      10000         f16cda
006400140307     c                   move      d1dcd2        f16cda
006500140307     c     d1dca1        mult      10000         f16ca
006600140307     c                   move      D1DCA2        f16ca
006700140307     c                   z-add     DalAmg        f16dti
006800140307     c                   z-add     AlAmg         f16dtf
006900140307     c                   z-add     d1PO          f16PO
007000140307     c                   move      d1PrcMes      f16PrcMes
007100140307     c                   move      D1emiss       f16stafil
007200140307     c                   move      D1test        f16test
007300140311     c                   move      D1scom        f16scom
007400140311     c                   move      D1scop        f16scop
007500140320     c                   z-add     d1primac      f16primac
007600140320     c                   z-add     d1oraokc      f16oraokc
007700140429     c                   clear                   kqieb
007800140429     c                   clear                   kqebi
007900140307     c                   movel(p)  fnlvp16ds     kpjbu
008000140307     c                   move      'LVP7'        kcoaz
008100030116      * Lancio Batch
008200140428     c                   if        *inkv
008300140428     C                   call      'BCH09'
008400140428     C                   parm                    KPJBA
008500140428     c                   endif
008600030825     c                   if        knmus = *all'1'
008700140307     c                   call      'FNLVP17C'
008800030124     c                   parm                    kpjba
008900030124     c                   else
009000020626     c                   call      'BCH10'
009100020624     c                   parm                    kpjba
009200030124     c                   end
009300120329     c                   exsr      srinz
009400020308     c                   endif
009500000000      *
009600120329     C                   enddo
009700030116      *
009800120329     C                   SETON                                        LR
009900120329      *--------------------------------------------------------------*
010000120329      * Routine per inizializzare i campi
010100120329      *--------------------------------------------------------------*
010200120329     c     srinz         begsr
010300140310     C                   MOVEL     dutPOU        d1PO
010400120329     C                   Z-ADD     d1PO          D1DCD1
010500120329     C                   Z-ADD     d1PO          d1dca1
010600120329      * Inizializzo i campi a video
010700120329     C                   move      *ZERO         d1dcd2
010800120329     C                   move      *All'9'       D1DCA2
010900120329     C                   Z-ADD     *ZERO         D1DDAL
011000120329     C                   Z-ADD     *ZERO         D1DAL
011100140304     C                   move      'N'           d1test
011200120619      * In FILIALE sempre la STAMPA
011300120619     C  N14              move      'S'           d1emiss
011400120619      * In  SEDE   propone il File di estrazione
011500120619     C   14              move      'F'           d1emiss
011600130122     C*-------
011700130122     C* Aggancia Organigramma x picking su messaggerie
011800130122     c                   clear                   og150
011900130122     c                   setoff                                       08
012000140312     c                   if        d1po <> 999
012100130122     C     d1PO          chain     AZORG01l
012200130122    1C                   IF        %found(azorg01l)
012300130122     c                   movel     orgdf0        og150
012400130122     c* p.o. con piking messaggerie
012500130122     c                   if        §ogpkm='S'
012600130122     c                   seton                                        08
012700130122     c                   endif
012800130122     c                   end
012900140312     c                   end
013000130122      *
013100130122      * sempre impostato su Tutto
013200130122     C                   move      'T'           d1prcmes
013300140320     c                   clear                   d1primac
013400140320     c                   clear                   d1oraokc
013500140311     C                   eval      d1scom= 30
013600140311     C                   eval      d1scop= 120
013700140314     c     ktbe          chain     tntbe01l
013800140314     c                   if        %found(tntbe01l) and tbeatb = ' '
013900140314     c                   movel     tbeuni        DVPOORARIS
014000140319     C                   eval      d1scom= §VPORST_Ms
014100140319     C                   eval      d1scop= §VPORST_Ps
014200140314     c                   end
014300130122     C*-------
014400120329     c                   endsr
014500000000      *--------------------------------------------------------------*
014600000000      * Routine per i controlli sui campi del formato video          *
014700000000      *--------------------------------------------------------------*
014800000000     C     CONTR         BEGSR
014900000000      *
015000140307     c                   movea     '000000000000'*in(39)
015100030116     c                   clear                   d1ccd1
015200030116     c                   clear                   d1ccd2
015300140312     c                   if        d1po <> 999
015400120330     C                   Z-ADD     d1PO          D1DCD1
015500120330     C                   Z-ADD     d1PO          d1dca1
015600140312     c                   else
015700140312     C                   Z-ADD     1             D1DCD1
015800140312     C                   Z-ADD     d1PO          d1dca1
015900140312     c                   end
016000030116      * --------------
016100131009     c                   if        utente_EDP = 'N'
016200131008     c****
016300140310     C                   IF        dutPOU <> 46
016400101018      * Controllo L'utente è abilitato alla Filiale
016500170327      /free
016600170327        IF  %lookup(%editc(d1po:'X'):skpoel) = 0;
016700170327      /end-free
016800101018     c                   seton                                        5090
016900120329     C                   leavesr
017000101018     C                   END
017100101022     C                   END
017200131008     c****
017300131008     C                   END
017400130122     C*-------
017500130122     C* Aggancia Organigramma x picking su messaggerie
017600130122     c                   clear                   og150
017700130122     c                   setoff                                       08
017800140312     c                   if        d1po <> 999
017900130122     C     d1PO          chain     AZORG01l
018000130122    1C                   IF        %found(azorg01l)
018100130122     c                   movel     orgdf0        og150
018200130122     c* p.o. con piking messaggerie
018300130122     c                   if        §ogpkm='S'
018400130122     c                   seton                                        08
018500130122     c                   endif
018600130122     c                   end
018700140312     c                   end
018800130122     C  N08              move      'T'           d1prcmes
018900101018      * --------------
019000101018      * Controllo range
019100030117     c     D1DCD1        mult      10000         fld701            7 0
019200030120     c                   move      d1dcd2        fld701
019300030116      *
019400030120     c     d1dca1        mult      10000         fld702            7 0
019500030117     c                   move      D1DCA2        fld702
019600030116      *
019700030116     C     fld702        iflt      fld701
019800030116     C                   SETON                                        4990
019900120329     C                   leavesr
020000030116     C                   END
020100030116      * --------------
020200030825      * Decodifiche Aut
020300131008      *
020400030825     c                   if        D1DCD2 <> 0
020500030116     C                   move      fld701        kpdr              7 0
020600030116     C     Kapd          chain     fiapd01l
020700030116     c                   if        not %Found(fiapd01l)
020800030116     C                   SETON                                        4790
020900120329     C                   leavesr
021000030116     C                   Else
021100100223     C                   MOVEL     apdrsf        d1ccd1
021200030116     C                   End
021300030120     C                   EndIf
021400030116      *
021500030825     c                   if        D1DCA2 <> 9999
021600030116     C                   move      fld702        kpdr
021700030116     C     Kapd          chain     fiapd01l
021800030116     c                   if        not %Found(fiapd01l)
021900030116     C                   SETON                                        4890
022000120329     C                   leavesr
022100030116     C                   Else
022200100223     C                   MOVEL     apdrsf        d1ccd2
022300030116     C                   End
022400030116     C                   EndIf
022500131008      *
022600030116      * --------------
022700020624      * controllo immissione delle date
022800030116     C     d1DDAL        ifeq      0
022900020308     C                   SETON                                        4390
023000120329     C                   leavesr
023100030116     C                   Else
023200030116     C                   Z-ADD     d1DDAL        G02DAT
023300000000     C                   MOVEL     *BLANK        G02ERR
023400940926     C                   CALL      'XSRDA8'
023500000000     C                   PARM                    WLBDAT
023600000000     C     G02ERR        IFEQ      '1'
023700020308     C                   SETON                                        44  90
023800120329     C                   leavesr
023900000000     C                   ENDIF
024000030116     C                   Z-ADD     G02INV        DalAmg            8 0
024100030116     C                   Z-ADD     G02dat        D1ddal
024200020307     c                   endif
024300120330     c* se la data inizio è minore della pulizia errore
024400120330     c                   if        dalamg < datpul
024500120330     C                   SETON                                        46  90
024600120330     C                   leavesr
024700120330     C                   ENDIF
024800030825      *
024900020624      * Controllo data finale
025000030116     C     d1Dal         ifeq      0
025100030127     C                   Z-ADD     d1DDal        d1Dal
025200030127     C                   End
025300030825      *
025400030116     C                   Z-ADD     d1Dal         G02DAT
025500020624     C                   MOVEL     *BLANK        G02ERR
025600020624     C                   CALL      'XSRDA8'
025700020624     C                   PARM                    WLBDAT
025800020624     C     G02ERR        IFEQ      '1'
025900020624     C                   SETON                                        42  90
026000120329     C                   leavesr
026100020624     C                   ENDIF
026200030825     c*
026300030116     C                   Z-ADD     G02INV        AlAmG             8 0
026400030116     C                   Z-ADD     G02dat        d1dal
026500030127     c*
026600030116     C     DalAmg        ifgt      AlAmg
026700020624     C                   SETON                                        4590
026800120329     C                   leavesr
026900020624     C                   END
027000120402     c* se un codice aut, + gg
027100120402     c* se + codici aut, max 1 gg
027200120402     c                   if        fld702 <>fld701 and
027300120402     C                             DalAmg <> AlAmg
027400131007     c                              and
027500140307     c                             utente_EDP = 'N' and
027600140310     c                             dutpou <> 046
027700120402     C                   SETON                                        3990
027800120402     C                   leavesr
027900120402     c                   end
028000140312     c                   if        d1po = 999 and
028100140307     c                             utente_EDP = 'N' and
028200140307     c                             d1emiss = 'S' and
028300140310     c                             dutpou = 046
028400140307     C                   SETON                                        4090
028500140307     C                   leavesr
028600140307     c                   end
028700030129      *
028800120329     C                   ENDSR
028900120329      *****************************************************************
029000120329     c     *inzsr        begsr
029100120329      *****************************************************************
029200120329     C     *ENTRY        PLIST
029300120329     C                   PARM                    KPJBA
029400120330     c*
029500120330     C     Ktab          KLIST
029600120330     C                   KFLD                    tblkut
029700120330     C                   KFLD                    tblcod
029800120330     C                   KFLD                    tblkey
029900120329     c*
030000140314     C     Ktbe          KLIST
030100140314     C                   KFLD                    tbecod
030200140314     C                   KFLD                    tbeke1
030300140314     c                   eval      tbecod = 'VPO'
030400140314     c                   eval      tbeke1 = 'ORARISER'
030500140314     c*
030600120329     C     Kapd          KLIST
030700120330     C                   KFLD                    D1ctip
030800120329     C                   KFLD                    kpdr              7 0
030900120329     C                   move      'A'           D1ctip            1
031000120521     c     *dtaara       define    §azute        azuteds
031100120521     c     *dtaara       define    §datiute      ddatiute
031200120521     c                   in(e)     *dtaara
031300120521     c                   if        %error  or rsut = *blanks
031400120521     c                   clear                   tibs34ds
031500120521     c                   call      'TIBS34R'
031600120521     c                   parm                    tibs34ds
031700120521     c                   in        *dtaara
031800120521     c                   endif
031900120330      *  In base all'utente prende il P.O.
032000120330     C                   CLEAR                   Tibs36Ds
032100120330     c                   EVAL      I36ute = Knmus
032200120330     c                   EVAL      I36Tla = 'L'
032300120330     C                   CALL      'TIBS36R'
032400120330     C                   PARM                    Tibs36Ds
032500120329      *
032600120619     c                   setoff                                       14
032700140310     c                   if        dutpou = 046
032800120619      * se in Sede 14 = *ON
032900120619     c                   seton                                        14
033000120619     c                   end
033100131007      *
033200131007      *   utente EDP può chiedere un range non limitato di selezione
033300131007      *      invece
033400131008      *    gli altri utenti possono chiedere in alternativa
033500131008      *      più giorni per un solo AUT
033600131008      *    oppure
033700131008      *      più AUT per una sola giornata.
033800131007     c                   movel     Knmus         EDPUTE            3
033900131007     c                   eval      utente_EDP = 'N'
034000131008     c                   move      '0'           Hin01             1
034100131007      * controlla se si tratta di utente "EDP"
034200131007     c                   if          EDPUTE ='EDP'
034300131008     c                   move      '1'           Hin01             1
034400131007     c                   eval      utente_EDP = 'S'
034500131007     c                   end
034600170327      /free
034700170327       //?Carico le filiali con abilitazione 'RC' Raggruppamento Centralino
034800170327       //?se utente di filiale
034900170327         IF  not *in14;
035000170327           clear TRUL31DS;
035100170327           I31abi = 'RC';
035200170327           I31cdi = DUTdis;
035300170327           I31car = DUTare;
035400170327           I31cpo = DUTpou;
035500170327           TRUL31R (kpjba:trul31ds);
035600170327       //?Carico le tutte le filiali
035700170327       //?se utente di sede
035800170327         ELSE;
035900170327           clear TRUL31DS;
036000170327           I31abi = 'AZ';
036100170327           I31cdi = DUTdis;
036200170327           I31car = DUTare;
036300170327           I31cpo = DUTpou;
036400170327           TRUL31R (kpjba:trul31ds);
036500170327         ENDIF;
036600170327      /end-free
036700131008     C***
036800120330     c* tabella 5a per pulizie
036900120330     c                   eval      tblkut = 1
037000120330     c                   eval      tblcod = '5A'
037100120330     c                   eval      tblkey = '2'
037200120330     c     ktab          chain     tabel00f
037300120330     c                   if        %found(tabel00f)
037400120330     c                   movel     tbluni        ds5a2
037500120330     c                   else
037600120330     c                   clear                   ds5a2
037700120330     c                   end
037800131008     C***
037900120330     C* Imposta la UDATE  da TIME
038000120330     C                   TIME                    W0140            14 0
038100120330     C                   MOVE      W0140         UDATE8            8 0
038200120330     c                   move      udate8        dataeur
038300120330     c                   move      dataeur       dataiso
038400120330     c                   move      dataiso       udate8g           8 0
038500120330     c*
038600120330     c     dataiso       subdur    §5apdal:*d    datapul
038700120330     c                   move      datapul       datpul            8 0
038800120329     c                   endsr
