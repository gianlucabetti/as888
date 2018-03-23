000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300131209      *       prompt di lancio stampa storico LOG PDA a TFNLVP11S
000400000000      *****************************************************************
000500130122     FAZORG01L  IF   E           K DISK
000600120402     Ftabel00f  IF   E           K DISK
000700170704     Ftfnlvp5L  uF a E           K DISK
000800120330     Ffiapd01L  IF   E           K DISK
000900131209     Ffnlvp14d  CF   E             WORKSTN
001000030116      *****************************************************************
001100030116     d KPJBA         E DS
001200120330     D Tibs36ds      E DS
001300131213     d ds5A4         e ds
001400170718     d** ds5A5         e ds
001500170718     d** ds5A6         e ds
001600131209     D fnlvp14ds     e DS                  inz
001700101018     D*-----------
001800000000     D WLBDAT          DS
001900940926     D  G02DAT                 1      8  0
002000940926     D  G02INV                 9     16  0
002100940926     D  G02ERR                17     17
002200940926     D  G02TGI                18     22  0
002300030205      *
002400120521     d azuteds       e ds                  extname(azute00f)
002500120521     d ddatiute      e ds
002600120521     d tibs34ds      e ds                  inz
002700130122     d og150         e ds
002701170718     d oggiiso         s               d   datfmt(*iso)
002800100617     d dataiso         s               d   datfmt(*iso)
002900100617     d dataeur         s               d   datfmt(*eur)
003000120330     d datapul         s               d   datfmt(*iso)
003100131212     d d1dsto8         s              8s 0
003200131212     d d1asto8         s              8s 0
003300170705     D*** fila            S              3    DIM(29)
003400170705     D*** filB            S              3    DIM(29)
003501170718      *--------------------------------------------------------------*
003600000000      *             M A I N      L I N E
003700000000      *--------------------------------------------------------------*
003800120329     c                   exsr      srinz
003900000000      * Emetto il formato video
004000120329     C                   do        *hival
004100020308     C                   SETOFF                                       90
004200131209     C                   EXFMT     CN14D01
004300120329     C   KC              leave
004400170707     C   KB              exsr      F02SR
004500000000      * Eseguo i  controlli sui campi immessi a video
004600000000     C                   EXSR      CONTR
004700120329     C   90              iter
004800000000      * Per errore generico riemetto il formato video
004900020307      * Richiamo programma di stampa
005000020308     c                   if        *inkf
005100131209     c                   clear                   fnlvp14ds
005200030116      * compone il codice numerico
005300131211     c     d1DAL10       mult      10000         f14cda
005400131211     c                   move      D1DAL11       f14cda
005500131211     c     D1AL20        mult      10000         f14ca
005600131211     c                   move      D1AL21        f14ca
005700131209     c                   z-add     DalAmg        f14dti
005800131209     c                   z-add     AlAmg         f14dtf
005900131209     c                   movel(p)  fnlvp14ds     kpjbu
006000131209     c                   move      'LVP5'        kcoaz
006100030116      * Lancio Batch
006200030825     c                   if        knmus = *all'1'
006300131209     c                   call      'FNLVP15R'
006400030124     c                   parm                    kpjba
006500030124     c                   else
006600020626     c                   call      'BCH10'
006700020624     c                   parm                    kpjba
006800030124     c                   end
006900120329     c                   exsr      srinz
007000020308     c                   endif
007100000000      *
007200120329     C                   enddo
007300030116      *
007400120329     C                   SETON                                        LR
007500120329      *--------------------------------------------------------------*
007600120329      * Routine per inizializzare i campi
007700120329      *--------------------------------------------------------------*
007800120329     c     srinz         begsr
007900131213     c*
008000131213      *    Al momento si vuole far prima chiodando una data di inizio
008100131213      *    storico e fino al preso dalla tabella
008200131213     C****
008300131213     c*
008400131213      *
008500131213      * tabella 5a per pulizie
008501170718     c                   clear                   ds5a4
008600131213     c                   eval      tblkut = 1
008700131213     c                   eval      tblcod = '5A'
008800131213     c                   eval      tblkey = '4'
008900131213     c                   eval      h1dsto  = 20131028
009000131213     c                   eval      d1dsto8 = 20131028
009100131213     c                   eval      h1asto  = 20391231
009200131213     c                   eval      d1asto8 = 20391231
009201170718     c                   eval      dataLimit = 19991231
009300131213     c     ktab          chain     tabel00f
009400131213     c                   if        %found(tabel00f)
009500131213     c                   movel     tblUNI        ds5a4
009600131213     c                   eval      h1asto  = §5aPLG_DAL
009700131213     c                   eval      d1asto8 = §5aPLG_DAL
009702170718     c                   end
009703170718      **
009704170718     c                   If         §5A4sPDAF = 0
009705170718     c                   EVAL       §5A4sPDAF = *all'9'
009706170718     c                   eND
009707170718      *
009708170718     c     oggiiso       subdur    §5A4sPDAf:*d  dataiso
009709170718     c                   move      dataiso       dataLimit         8 0
009900170704      **
010000170704     c                   goto      salta
010100170704      **
010200150107      *filiali da salvare tabella 5A key 5
010300170705     c***                   clear                   fila
010400170705     c***                   clear                   filb
010500170705     c***                   eval      tblkut = 1
010600170705     c***                   eval      tblcod = '5A'
010700170705     c***                   eval      tblkey = '5'
010800170705     c***     ktab          chain     tabel00f
010900170705     c***                   if        %found(tabel00f)
011000170705     c***                   movel     tblUNI        ds5a5
011100170705     c***                   movea     §5AFILA       fila
011200170705     c***                   end
011300170705     c***                   eval      tblkey = '6'
011400170705     c***     ktab          chain     tabel00f
011500170705     c***                   if        %found(tabel00f)
011600170705     c***                   movel     tblUNI        ds5a6
011700170705     c***                   movea     §6AFILA       filB
011800170705     c***                   end
011900170705      ***
012000131212     c/exec sql
012100131212     c+ SELECT min(p11wddc), min(p11wddc),
012200131212     c+        max(p11wddc), max(p11wddc)
012300131212     c+ INTO  :h1dsto, :d1dSTO8, :h1asto, :d1aSTO8
012400131212     c+ FROM tfnlvp11s
012500131212     c/end-exec
012600131213      *
012700131213     c     salta         tag
012800131213      *
012900131212     C     *iso          move      d1dsto8       dataiso
013000131212     C                   move      dataiso       dataeur
013100131212     C                   move      dataeur       d1dsto
013200131212     C                   move      d1asto8       dataiso
013300131212     C                   move      dataiso       dataeur
013400131212     C                   move      dataeur       d1asto
013500131212      *
013600131211     C                   Z-ADD     0             d1DAL10
013700131211     C                   Z-ADD     999           D1AL20
013800120329      * Inizializzo i campi a video
013900131211     C                   move      *ZERO         D1DAL11
014000131211     C                   move      *All'9'       D1AL21
014100131211     C                   Z-ADD     *ZERO         D1DAL
014200131211     C                   Z-ADD     *ZERO         D1AL
014300170704     C                   clear                   d1msg
014400170704     c                   setoff                                       98
014500130122     C*-------
014600120329     c                   endsr
014700000000      *--------------------------------------------------------------*
014800170707      *   Richiamo al pgm delle filiali da NON pulire                *
014900000000      *--------------------------------------------------------------*
015000170707     C     F02SR         BEGSR
015100000000      *
015200170707     c                   call      'FNLVP18R'
015300170707     c                   parm                    kpjba
015400170707      *
015500170707     c                   endsr
015600170707      *--------------------------------------------------------------*
015700170707      * Routine per i controlli sui campi del formato video          *
015800170707      *--------------------------------------------------------------*
015900170707     C     CONTR         BEGSR
016000170707      *
016100030116     c                   movea     '0000000000'  *in(41)
016200131211     c                   clear                   D1DAL1D
016300131211     c                   clear                   D1AL2D
016400030116      * --------------
016500020624      * controllo immissione delle date
016600131211     C     d1DAL         ifeq      0
016700020308     C                   SETON                                        4390
016800120329     C                   leavesr
016900030116     C                   Else
017000131211     C                   Z-ADD     d1DAL         G02DAT
017100000000     C                   MOVEL     *BLANK        G02ERR
017200940926     C                   CALL      'XSRDA8'
017300000000     C                   PARM                    WLBDAT
017400000000     C     G02ERR        IFEQ      '1'
017500020308     C                   SETON                                        44  90
017600120329     C                   leavesr
017700000000     C                   ENDIF
017800030116     C                   Z-ADD     G02INV        DalAmg            8 0
017900131211     C                   Z-ADD     G02dat        D1dal
018000020307     c                   endif
018100030825      *
018200020624      * Controllo data finale
018300131211     C     d1al          ifeq      0
018400131211     C                   Z-ADD     d1Dal         d1al
018500030127     C                   End
018600030825      *
018700131211     C                   Z-ADD     d1al          G02DAT
018800020624     C                   MOVEL     *BLANK        G02ERR
018900020624     C                   CALL      'XSRDA8'
019000020624     C                   PARM                    WLBDAT
019100020624     C     G02ERR        IFEQ      '1'
019200020624     C                   SETON                                        42  90
019300120329     C                   leavesr
019400020624     C                   ENDIF
019500030825     c*
019600030116     C                   Z-ADD     G02INV        AlAmG             8 0
019700131211     C                   Z-ADD     G02dat        d1al
019800030127     c*
019900030116     C     DalAmg        ifgt      AlAmg
020000020624     C                   SETON                                        4590
020100120329     C                   leavesr
020200020624     C                   END
020300030129      *
020400131212     c*
020500131212     c*  Fuori range dello storico
020600131212     C     DalAmg        iflt      h1dSTO
020700131212     C                   SETON                                        5590
020800131212     C                   leavesr
020900131212     C                   END
021000131212      *
021100131212     c*  Fuori range dello storico
021200131212     C     AlAmg         ifgt      h1aSTO
021300131212     C                   SETON                                        5690
021400131212     C                   leavesr
021500131212     C                   END
021600131212      * --------------
021700131212     c                   move      d1DAL10       d1AL20
021800131212      * Controllo range
021900131212     c     d1DAL10       mult      10000         fld701            7 0
022000131212     c                   move      D1DAL11       fld701
022100131212      *
022200131212     c     D1AL20        mult      10000         fld702            7 0
022300131212     c                   move      D1AL21        fld702
022400131212      *
022500131212     C     fld702        iflt      fld701
022600131212     C                   SETON                                        4990
022700131212     C                   leavesr
022800131212     C                   END
022900131212      *
023000131212      *
023100131212     C     d1DAL10       ifeq      0
023200131212     C                   SETON                                        4690
023300131212     C                   leavesr
023400131212     C                   END
023500150107     c                   movel     d1dal10       filaut            3
023600150107     c                   setoff                                       98
023700170704     c***     filaut        lookup    fila                                   99
023800170704     c***  n99filaut        lookup    filb                                   99
023900170704     c***  n99              seton                                        98
024000170704      * --------------
024100170704      * Se non presente in tabella la Filiale da NON cancellare
024101170718      * oppure c'è ma la data + più vecchia del limite quindi come se
024102170718      * non ci fosse più
024200170704     c                   movel     d1dal10       fil03             3 0
024300170704     c     fil03         chain     tfnlvp5L
024400170718     c                   if        not %Found(tfnlvp5L) OR
024401170718     c                             %Found(tfnlvp5L) and VP5DIMM < datalimit
024500170704     c                   seton                                        98
024600170704     c                   end
024700170704      *
024800170704      *** al F6 di conferma
024900170704      ** il controllo serve anche per scrivere il record filiale alla data
025000170704      ***  oppure per aggiornarlo alla data
025100170704     c                   if        *inKF
025200170704     c                   if        not %Found(tfnlvp5L)
025300170704     c                   move      fil03         VP5FGS
025400170704     c                   z-add     udate8g       VP5DIMM
025500170704     c                   write     TFNLVP5
025600170704     c                   else
025700170704     c                   z-add     udate8g       VP5DIMM
025800170704     c                   update    TFNLVP5
025900170704     c                   end
026000170704     c                   end
026100140310      * --------------
026200140310      * Decodifiche Aut
026300140310      *
026400140310     c                   if        D1DAL11 <> 0
026500140310     C                   move      fld701        kpdr              7 0
026600140310     C     Kapd          chain     fiapd01l
026700140310     c                   if        not %Found(fiapd01l)
026800140310     C*******            SETON                                        4790
026900140310     C*******            leavesr
027000140310     C                   Else
027100140310     C                   MOVEL     apdrsf        D1DAL1D
027200140310     C                   End
027300140310     C                   EndIf
027400140310      *
027500140310     c                   if        D1AL21 <> 9999
027600140310     C                   move      fld702        kpdr
027700140310     C     Kapd          chain     fiapd01l
027800140310     c                   if        not %Found(fiapd01l)
027900140310     C*******            SETON                                        4890
028000140310     C*******            leavesr
028100140310     C                   Else
028200140310     C                   MOVEL     apdrsf        D1AL2D
028300140310     C                   End
028400140310     C                   EndIf
028500140310      *
028600170704      * --------------
028700170704      *   controlla la presenza di record per la stampa altrimenti segnala
028800170704     c                   z-add     0             totale           15 0
028900170704     c                   clear                   d1MSG
029000170704      *
029100170704     C/EXEC SQL
029200170704     C+ SELECT count(*) INTO :totale FROM
029300170704     C+    TFNLVP11S
029400170704     C+  WHERE P11WDDC  between :DalAmg  and :alAmg
029500170704     C+   and  P11CAUT  between :fld701  and :fld702
029600170704     C/END-EXEC
029700170704      *
029800170704     c                   if        totale = 0
029900170704     c                   eval      d1MSG = 'ATTENZIONE nessun dato x la -
030000170704     c                             SELEZIONE - Modificare i parametri !!!!'
030100170704     c                   end
030200170704      *
030300120329     C                   ENDSR
030400120329      *****************************************************************
030500120329     c     *inzsr        begsr
030600120329      *****************************************************************
030700120329     C     *ENTRY        PLIST
030800120329     C                   PARM                    KPJBA
030900120330     c*
031000120330     C     Ktab          KLIST
031100120330     C                   KFLD                    tblkut
031200120330     C                   KFLD                    tblcod
031300120330     C                   KFLD                    tblkey
031400120329     c*
031500120329     C     Kapd          KLIST
031600120330     C                   KFLD                    D1ctip
031700120329     C                   KFLD                    kpdr              7 0
031800120329     C                   move      'A'           D1ctip            1
031900131209      **
032000120521     c     *dtaara       define    §azute        azuteds
032100120521     c     *dtaara       define    §datiute      ddatiute
032200120521     c                   in(e)     *dtaara
032300120521     c                   if        %error  or rsut = *blanks
032400120521     c                   clear                   tibs34ds
032500120521     c                   call      'TIBS34R'
032600120521     c                   parm                    tibs34ds
032700120521     c                   in        *dtaara
032800120521     c                   endif
032900120330      *  In base all'utente prende il P.O.
033000120330     C                   CLEAR                   Tibs36Ds
033100120330     c                   EVAL      I36ute = Knmus
033200120330     c                   EVAL      I36Tla = 'L'
033300120330     C                   CALL      'TIBS36R'
033400120330     C                   PARM                    Tibs36Ds
033500120329      *
033600131008     C***
033700120330     C* Imposta la UDATE  da TIME
033800120330     C                   TIME                    W0140            14 0
033900120330     C                   MOVE      W0140         UDATE8            8 0
034000120330     c                   move      udate8        dataeur
034100120330     c                   move      dataeur       dataiso
034200120330     c                   move      dataiso       udate8g           8 0
034300170718     c                   move      dataiso       oggiiso
034302170718      **
034303170718      *
034400120329     c                   endsr
