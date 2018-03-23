000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300131209      *       prompt di lancio stampa storico LOG PDA a TFNLVP11S
000400000000      *****************************************************************
000500130122     FAZORG01L  IF   E           K DISK
000600120402     Ftabel00f  IF   E           K DISK
000700120330     Ffiapd01L  IF   E           K DISK
000800131209     Ffnlvp14d  CF   E             WORKSTN
000900030116      *****************************************************************
001000030116     d KPJBA         E DS
001100120330     D Tibs36ds      E DS
001200131213     d ds5A4         e ds
001201150107     d ds5A5         e ds
001300131209     D fnlvp14ds     e DS                  inz
001400101018     D*-----------
001500000000     D WLBDAT          DS
001600940926     D  G02DAT                 1      8  0
001700940926     D  G02INV                 9     16  0
001800940926     D  G02ERR                17     17
001900940926     D  G02TGI                18     22  0
002000030205      *
002100120521     d azuteds       e ds                  extname(azute00f)
002200120521     d ddatiute      e ds
002300120521     d tibs34ds      e ds                  inz
002400130122     d og150         e ds
002500100617     d dataiso         s               d   datfmt(*iso)
002600100617     d dataeur         s               d   datfmt(*eur)
002700120330     d datapul         s               d   datfmt(*iso)
002800131212     d d1dsto8         s              8s 0
002900131212     d d1asto8         s              8s 0
002901150107     D fila            S              3    DIM(29)
003000000000      *--------------------------------------------------------------*
003100000000      *             M A I N      L I N E
003200000000      *--------------------------------------------------------------*
003300120329     c                   exsr      srinz
003400000000      * Emetto il formato video
003500120329     C                   do        *hival
003600020308     C                   SETOFF                                       90
003700131209     C                   EXFMT     CN14D01
003800120329     C   KC              leave
003900000000      * Eseguo i  controlli sui campi immessi a video
004000000000     C                   EXSR      CONTR
004100120329     C   90              iter
004200000000      * Per errore generico riemetto il formato video
004300020307      * Richiamo programma di stampa
004400020308     c                   if        *inkf
004500131209     c                   clear                   fnlvp14ds
004600030116      * compone il codice numerico
004700131211     c     d1DAL10       mult      10000         f14cda
004800131211     c                   move      D1DAL11       f14cda
004900131211     c     D1AL20        mult      10000         f14ca
005000131211     c                   move      D1AL21        f14ca
005100131209     c                   z-add     DalAmg        f14dti
005200131209     c                   z-add     AlAmg         f14dtf
005300131209     c                   movel(p)  fnlvp14ds     kpjbu
005400131209     c                   move      'LVP5'        kcoaz
005500030116      * Lancio Batch
005600030825     c                   if        knmus = *all'1'
005700131209     c                   call      'FNLVP15R'
005800030124     c                   parm                    kpjba
005900030124     c                   else
006000020626     c                   call      'BCH10'
006100020624     c                   parm                    kpjba
006200030124     c                   end
006300120329     c                   exsr      srinz
006400020308     c                   endif
006500000000      *
006600120329     C                   enddo
006700030116      *
006800120329     C                   SETON                                        LR
006900120329      *--------------------------------------------------------------*
007000120329      * Routine per inizializzare i campi
007100120329      *--------------------------------------------------------------*
007200120329     c     srinz         begsr
007300131213     c*
007400131213      *    Al momento si vuole far prima chiodando una data di inizio
007500131213      *    storico e fino al preso dalla tabella
007600131213     C****
007700131213     c*
007800131213      *
007900131213      * tabella 5a per pulizie
008000131213     c                   eval      tblkut = 1
008100131213     c                   eval      tblcod = '5A'
008200131213     c                   eval      tblkey = '4'
008300131213     c                   eval      h1dsto  = 20131028
008400131213     c                   eval      d1dsto8 = 20131028
008500131213     c                   eval      h1asto  = 20391231
008600131213     c                   eval      d1asto8 = 20391231
008700131213     c     ktab          chain     tabel00f
008800131213     c                   if        %found(tabel00f)
008900131213     c                   movel     tblUNI        ds5a4
009000131213     c                   eval      h1asto  = §5aPLG_DAL
009100131213     c                   eval      d1asto8 = §5aPLG_DAL
009200131213     c                   end
009300150107      *filiali da salvare tabella 5A key 5
009301150107     c                   clear                   fila
009302150107     c                   eval      tblkut = 1
009303150107     c                   eval      tblcod = '5A'
009304150107     c                   eval      tblkey = '5'
009308150107     c     ktab          chain     tabel00f
009309150107     c                   if        %found(tabel00f)
009310150107     c                   movel     tblUNI        ds5a5
009311150107     c                   movea     §5AFILA       fila
009313150107     c                   end
009400131213     c                   goto      salta
009500131212      *
009600131212     c/exec sql
009700131212     c+ SELECT min(p11wddc), min(p11wddc),
009800131212     c+        max(p11wddc), max(p11wddc)
009900131212     c+ INTO  :h1dsto, :d1dSTO8, :h1asto, :d1aSTO8
010000131212     c+ FROM tfnlvp11s
010100131212     c/end-exec
010200131213      *
010300131213     c     salta         tag
010400131213      *
010500131212     C     *iso          move      d1dsto8       dataiso
010600131212     C                   move      dataiso       dataeur
010700131212     C                   move      dataeur       d1dsto
010800131212     C                   move      d1asto8       dataiso
010900131212     C                   move      dataiso       dataeur
011000131212     C                   move      dataeur       d1asto
011100131212      *
011200131211     C                   Z-ADD     0             d1DAL10
011300131211     C                   Z-ADD     999           D1AL20
011400120329      * Inizializzo i campi a video
011500131211     C                   move      *ZERO         D1DAL11
011600131211     C                   move      *All'9'       D1AL21
011700131211     C                   Z-ADD     *ZERO         D1DAL
011800131211     C                   Z-ADD     *ZERO         D1AL
011900130122     C*-------
012000120329     c                   endsr
012100000000      *--------------------------------------------------------------*
012200000000      * Routine per i controlli sui campi del formato video          *
012300000000      *--------------------------------------------------------------*
012400000000     C     CONTR         BEGSR
012500000000      *
012600030116     c                   movea     '0000000000'  *in(41)
012700131211     c                   clear                   D1DAL1D
012800131211     c                   clear                   D1AL2D
012900030116      * --------------
013000020624      * controllo immissione delle date
013100131211     C     d1DAL         ifeq      0
013200020308     C                   SETON                                        4390
013300120329     C                   leavesr
013400030116     C                   Else
013500131211     C                   Z-ADD     d1DAL         G02DAT
013600000000     C                   MOVEL     *BLANK        G02ERR
013700940926     C                   CALL      'XSRDA8'
013800000000     C                   PARM                    WLBDAT
013900000000     C     G02ERR        IFEQ      '1'
014000020308     C                   SETON                                        44  90
014100120329     C                   leavesr
014200000000     C                   ENDIF
014300030116     C                   Z-ADD     G02INV        DalAmg            8 0
014400131211     C                   Z-ADD     G02dat        D1dal
014500020307     c                   endif
014600030825      *
014700020624      * Controllo data finale
014800131211     C     d1al          ifeq      0
014900131211     C                   Z-ADD     d1Dal         d1al
015000030127     C                   End
015100030825      *
015200131211     C                   Z-ADD     d1al          G02DAT
015300020624     C                   MOVEL     *BLANK        G02ERR
015400020624     C                   CALL      'XSRDA8'
015500020624     C                   PARM                    WLBDAT
015600020624     C     G02ERR        IFEQ      '1'
015700020624     C                   SETON                                        42  90
015800120329     C                   leavesr
015900020624     C                   ENDIF
016000030825     c*
016100030116     C                   Z-ADD     G02INV        AlAmG             8 0
016200131211     C                   Z-ADD     G02dat        d1al
016300030127     c*
016400030116     C     DalAmg        ifgt      AlAmg
016500020624     C                   SETON                                        4590
016600120329     C                   leavesr
016700020624     C                   END
016800030129      *
016900131212     c*
017000131212     c*  Fuori range dello storico
017100131212     C     DalAmg        iflt      h1dSTO
017200131212     C                   SETON                                        5590
017300131212     C                   leavesr
017400131212     C                   END
017500131212      *
017600131212     c*  Fuori range dello storico
017700131212     C     AlAmg         ifgt      h1aSTO
017800131212     C                   SETON                                        5690
017900131212     C                   leavesr
018000131212     C                   END
018100131212      * --------------
018200131212     c                   move      d1DAL10       d1AL20
018300131212      * Controllo range
018400131212     c     d1DAL10       mult      10000         fld701            7 0
018500131212     c                   move      D1DAL11       fld701
018600131212      *
018700131212     c     D1AL20        mult      10000         fld702            7 0
018800131212     c                   move      D1AL21        fld702
018900131212      *
019000131212     C     fld702        iflt      fld701
019100131212     C                   SETON                                        4990
019200131212     C                   leavesr
019300131212     C                   END
019400131212      *
019500131212      *
019600131212     C     d1DAL10       ifeq      0
019700131212     C                   SETON                                        4690
019800131212     C                   leavesr
019900131212     C                   END
019901150107     c                   movel     d1dal10       filaut            3
019902150107     c                   setoff                                       98
019903150107     c     filaut        lookup    fila                                   99
019904150107     c  n99              seton                                        98
020000140310      * --------------
020100140310      * Decodifiche Aut
020200140310      *
020300140310     c                   if        D1DAL11 <> 0
020400140310     C                   move      fld701        kpdr              7 0
020500140310     C     Kapd          chain     fiapd01l
020600140310     c                   if        not %Found(fiapd01l)
020700140310     C*******            SETON                                        4790
020800140310     C*******            leavesr
020900140310     C                   Else
021000140310     C                   MOVEL     apdrsf        D1DAL1D
021100140310     C                   End
021200140310     C                   EndIf
021300140310      *
021400140310     c                   if        D1AL21 <> 9999
021500140310     C                   move      fld702        kpdr
021600140310     C     Kapd          chain     fiapd01l
021700140310     c                   if        not %Found(fiapd01l)
021800140310     C*******            SETON                                        4890
021900140310     C*******            leavesr
022000140310     C                   Else
022100140310     C                   MOVEL     apdrsf        D1AL2D
022200140310     C                   End
022300140310     C                   EndIf
022400140310      *
022500131212      *
022600120329     C                   ENDSR
022700120329      *****************************************************************
022800120329     c     *inzsr        begsr
022900120329      *****************************************************************
023000120329     C     *ENTRY        PLIST
023100120329     C                   PARM                    KPJBA
023200120330     c*
023300120330     C     Ktab          KLIST
023400120330     C                   KFLD                    tblkut
023500120330     C                   KFLD                    tblcod
023600120330     C                   KFLD                    tblkey
023700120329     c*
023800120329     C     Kapd          KLIST
023900120330     C                   KFLD                    D1ctip
024000120329     C                   KFLD                    kpdr              7 0
024100120329     C                   move      'A'           D1ctip            1
024200131209      **
024300120521     c     *dtaara       define    §azute        azuteds
024400120521     c     *dtaara       define    §datiute      ddatiute
024500120521     c                   in(e)     *dtaara
024600120521     c                   if        %error  or rsut = *blanks
024700120521     c                   clear                   tibs34ds
024800120521     c                   call      'TIBS34R'
024900120521     c                   parm                    tibs34ds
025000120521     c                   in        *dtaara
025100120521     c                   endif
025200120330      *  In base all'utente prende il P.O.
025300120330     C                   CLEAR                   Tibs36Ds
025400120330     c                   EVAL      I36ute = Knmus
025500120330     c                   EVAL      I36Tla = 'L'
025600120330     C                   CALL      'TIBS36R'
025700120330     C                   PARM                    Tibs36Ds
025800120329      *
025900131008     C***
026000120330     C* Imposta la UDATE  da TIME
026100120330     C                   TIME                    W0140            14 0
026200120330     C                   MOVE      W0140         UDATE8            8 0
026300120330     c                   move      udate8        dataeur
026400120330     c                   move      dataeur       dataiso
026500120330     c                   move      dataiso       udate8g           8 0
026600120330     c*
026700120329     c                   endsr
