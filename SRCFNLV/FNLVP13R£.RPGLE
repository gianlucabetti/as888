000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300131127      *       STORICIZZAZIONE records del PLG formato di Stampa
000400000000      *****************************************************************
000500130122     FAZORG01L  IF   E           K DISK
000600131127     Ftabel00f  uF   E           K DISK
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900101018     D*-----------
001000000000     D WLBDAT          DS
001100940926     D  G02DAT                 1      8  0
001200940926     D  G02INV                 9     16  0
001300940926     D  G02ERR                17     17
001400940926     D  G02TGI                18     22  0
001500131127      *
001600131127     D fnlvp10ds     e DS                  inz
001700131128     d ds5A4         e ds
001800131127      *
001900100617     d dataiso         s               d   datfmt(*iso)
002000100617     d dataeur         s               d   datfmt(*eur)
002100131127     d data_dal        s              8S 0
002200131127      *
002300131007     d utente_EDP      s              1
002400000000      *--------------------------------------------------------------*
002500000000      *             M A I N      L I N E
002600000000      *--------------------------------------------------------------*
002700000000      * Emetto il formato video
002800131127     C                   read      AZORG01L
002900131127      **
003000131127     C                   DOw       not %EoF(Azorg01L)
003100131127      **
003200131127      ** solo filiali attive
003300131127     c                   IF        orgFAG ='F' and orgFVA=' '
003400131127      **
003500131128     c                   move      'S'           storicizza        1
003600120329     c                   clear                   fnlvp10ds
003700131127      * compone il range Autisti e Range di date
003800131127     c     orgFIL        mult      10000         f10cda
003900131127     c                   z-add     F10cda        f10ca
004000131127     c                   add       9999          f10ca
004100131127     c                   z-add     Data_dal      f10dti
004200120329     c                   z-add     AlAmg         f10dtf
004300131127     c                   z-add     orgFIL        f10PO
004400131127     c                   move      'T'           f10PrcMes
004500131127     c                   move      'F'           f10stafil
004501140304     c                   move      'N'           f10test
004600120329     c                   movel(p)  fnlvp10ds     kpjbu
004700131127     c                   call      'FNLVP11R'
004800030124     c                   parm                    kpjba
004900131128     c                   parm                    storicizza
005000131127      **
005100131127     c                   endIF
005200000000      *
005300131127     C                   read      AZORG01L
005400131127     C                   endDO
005500131127     C****
005600131128     c     ktab          chain     tabel00f
005700131127     c                   if        %found(tabel00f)
005800131128     c                   movel     tblUNI        ds5a4
005900131127     C                   Z-ADD     udate8g       §5aPLG_DAL
006000131128     c                   EVAL        tblUNI = ds5a4
006100131127     c                   UPDATE    TABEL
006200131127     c                   end
006300030116      *
006400120329     C                   SETON                                        LR
006500120329      *****************************************************************
006600120329     c     *inzsr        begsr
006700120329      *****************************************************************
006800120329     C     *ENTRY        PLIST
006900120329     C                   PARM                    KPJBA
007000120330     c*
007100120330     C     Ktab          KLIST
007200120330     C                   KFLD                    tblkut
007300120330     C                   KFLD                    tblcod
007400120330     C                   KFLD                    tblkey
007500131127      *
007600131127      * tabella 5a per pulizie
007700120330     c                   eval      tblkut = 1
007800120330     c                   eval      tblcod = '5A'
007900131128     c                   eval      tblkey = '4'
008000131128     c                   clear                   ds5a4
008100131127     c     ktab          chain(n)  tabel00f
008200120330     c                   if        %found(tabel00f)
008300131128     c                   movel     tbluni        ds5a4
008400120330     c                   end
008500131127      ***
008600131127      * Imposta la UDATE  da TIME
008700120330     C                   TIME                    W0140            14 0
008800120330     C                   MOVE      W0140         UDATE8            8 0
008900120330     c                   move      udate8        dataeur
009000120330     c                   move      dataeur       dataiso
009100120330     c                   move      dataiso       udate8g           8 0
009200131127     C                   Z-ADD     udate8g       AlAmG             8 0
009300131127      *
009400131127      *  ultima data giorno storicizzato + 1gg.
009500131128     c     *iso          move      §5aPLG_DAL    dataiso
009600131127     c     dataiso       adddur    1:*d          dataiso
009700131127     c                   move      dataiso       data_dal          8 0
009800131127      *
009900120329     c                   endsr
010000131127      * --------------------------------------------------------------------
