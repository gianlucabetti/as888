000100100223     hDECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300030825      *       prompt di lancio stampa saldi di periodo
000400000000      *****************************************************************
000500030116     Ffiapd01L  IF   E           K DISK
000600030825     Fficn23D   CF   E             WORKSTN
000700030116      *****************************************************************
000800030116     d KPJBA         E DS
000900030120     D Tibs36ds      E DS
001000000000     D CNCR80        E DS
001100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200030825     D ficn23ds      e DS                  inz
001300100317     D ficn70ds      e DS                  inz
001400100617     d dvpocont      e ds
001500100617     d tibs02ds      e ds
001600100317     D daut          e DS
002300101018     D*-----------
002400000000     D WLBDAT          DS
002500940926     D  G02DAT                 1      8  0
002600940926     D  G02INV                 9     16  0
002700940926     D  G02ERR                17     17
002800940926     D  G02TGI                18     22  0
002900030205      *
003000030116     D digits          C                   CONST('0123456789')
003100100617     d dataiso         s               d   datfmt(*iso)
003200100617     d dataeur         s               d   datfmt(*eur)
003300030116      *****************************************************************
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600030217     c                   movel     kpjbu         tipo              1
003700030205     c*
003800030116     C     Kapd          KLIST
003900030116     C                   KFLD                    D1ctip
004000030116     C                   KFLD                    kpdr              7 0
004100030825     C                   move      'A'           D1ctip            1
004200030120      *
004300030825     C* Imposta la UDATE  da TIME
004400030825     C                   TIME                    W0140            14 0
004500030825     C                   MOVE      W0140         UDATE8            8 0
004600100617     c                   move      udate8        dataeur
004700100617     c                   move      dataeur       dataiso
004800100617     c                   move      dataiso       udate8g           8 0
004900030825     C*
005000030120      *  In base all'utente prende il P.O.
005100030120     C                   CLEAR                   Tibs36Ds
005200030120     c                   EVAL      I36ute = Knmus
005300030120     c                   EVAL      I36Tla = 'L'
005400030120     C                   CALL      'TIBS36R'
005500030120     C                   PARM                    Tibs36Ds
005600030827     c                   movel     KNMUS         ALFA3             3
005700030829     c                   setoff                                       0102
005800101018      *
005900030829      * solo per EDP....:
006000051124     c*                  if        ALFA3 ='EDP'
006100030829     c                   seton                                        01
006200051124     c*                  end
006300101018      *
006400030829      * per utenti di filiale:
006500101018     c                   if        ALFA3<>'EDP' and O36Pou <>46 and
006600101018     c                             ALFA3<>'REA'
006700101019     c************       seton                                        02
006800030829     c                   end
006900101018     C***
007000100617      * controllo se attivare o no controllo autorizzazioni
007100100617     c                   clear                   tibs02ds
007200100617     c                   clear                   dvpocont
007300100617     c                   eval      t02mod = 'C'
007400100617     c                   eval      t02sif = knsif
007500100617     c                   eval      t02cod = 'VPO'
007600100617     c                   eval      t02ke1 = 'CONT'
007700100617     c                   call      'TIBS02R'
007800100617     c                   parm                    kpjba
007900100617     c                   parm                    tibs02ds
008000100617    2c                   if        t02err = *blanks
008100100617     c                   eval      dvpocont  = t02uni
008200100617    2c                   endif
009600101018      *
009700000000      *--------------------------------------------------------------*
009800000000      *             M A I N      L I N E
009900000000      *--------------------------------------------------------------*
010000000000     C                   Z-ADD     1             CODUT
010100000000     C                   CALL      'X§PARUT'
010200000000     C                   PARM                    UT§DSE
010300000000     C                   MOVEL     RAGUT         RSUT             20
010400000000     C                   MOVEL     RAGUT         RSUT2            20
010500000000     C                   MOVEL     REC80         CNCR80
010600030116      *
010700030825     C                   MOVEL     o36POU        d1PO
012000030825      *
012100030116     C     INIZIO        tag
012200030825     C                   Z-ADD     d1PO          D1DCD1                         LNP
012300030825     C                   Z-ADD     d1PO          d1dca1                         LNP
012400030116      *
012500000000      * Inizializzo i campi a video
012600030825     C                   move      *ZERO         d1dcd2                         Cod
012700030825     C                   move      *All'9'       D1DCA2                         Cod
012800030116     C                   Z-ADD     *ZERO         D1DDAL                         DATA INIZIALE
012900030116     C                   Z-ADD     *ZERO         D1DAL                          DATA FINALE
013000030825      *
013100030217     c                   move      'S'           D1Dett
013200030827     c                   move      'N'           D1TGEN
013300030825      *
013400000000      * Emetto il formato video
013500030116     C     Video1        TAG
013600020308     C                   SETOFF                                       90
013700030825     C                   EXFMT     CN23D01
013800000000      *
013900000000     C   KC              GOTO      FINE                                         F3=USCITA
014000000000      *
014100000000      * Eseguo i  controlli sui campi immessi a video
014200000000     C                   EXSR      CONTR
014201130808      *
014300030829     C                   Z-ADD     d1PO          D1DCD1                         LNP
014400030829     C                   Z-ADD     d1PO          d1dca1                         LNP
014500000000      *
014600020308      * Se non vi sono errori ma non e' premuto F6 emetto video
014700020308     C  N90
014800020308     CanNkf              SETON                                          90      F6=CONFERMA
014900000000      *
015000000000      * Per errore generico riemetto il formato video
015100030116     C   90              GOTO      Video1
015200000000      *
015300020307      * Richiamo programma di stampa
015400020308     c                   if        *inkf
015500030825     c                   clear                   ficn23ds
015600030116      * compone il codice numerico
015700030825     c     D1DCD1        mult      10000         f23cda
015800030825     c                   move      d1dcd2        f23cda
015900030825     c     d1dca1        mult      10000         f23ca
016000030825     c                   move      D1DCA2        f23ca
016100030825     c                   z-add     DalAmg        f23dti
016200030825     c                   z-add     AlAmg         f23dtf
016300030825     c                   move      d1dett        f23det
016400030825     c                   z-add     d1PO          f23PO
016500030827     c                   move      d1tGen        f23gen
016600030116      *
016700030825     c                   movel(p)  ficn23ds      kpjbu
016800030825     c                   move      'FN24'        kcoaz
016900030116      * Lancio Batch
017000030825     c                   if        knmus = *all'1'
017100030825     c                   call      'FICN24R'
017200030124     c                   parm                    kpjba
017300030124     c                   else
017400020626     c                   call      'BCH10'
017500020624     c                   parm                    kpjba
017600030124     c                   end
017700020308     c                   endif
017800000000      *
017900030116     C                   GOTO      INIZIO
018000030116      *
018100000000     C     FINE          TAG
018200000000     C                   SETON                                        LR
018300000000      *--------------------------------------------------------------*
018400000000      * Routine per i controlli sui campi del formato video          *
018500000000      *--------------------------------------------------------------*
018600000000     C     CONTR         BEGSR
018700000000      *
018800030116     c                   movea     '0000000000'  *in(41)
018900030116     c                   clear                   d1ccd1
019000030116     c                   clear                   d1ccd2
019100030116      * --------------
019200101022     C                   IF        O36POU <> 46
019900101018      *
020000101018      * controllo autorizzazione del profilo ad accedere
020100101022     c                   if        udate8g >= §vpoaut
020200101018     c                   movel     o36pou        i70pog
020300101018     c                   movel     d1po          i70pge
020400101018     c                   movel     knmus         i70prf
020500101018     c                   call      'FICN70R'
020600101018     c                   parm                    ficn70ds
020700101018     c                   movel     o70uni        daut
020800101018     c                   if        §AUTSTDCC <> 'S'
020900101018     c                   seton                                        5090
021000101018     C                   GOTO      endct1
021100101018     c                   endif
021200101018     c                   endif
021300101018      *
021400101022     C                   END
021500101018      * --------------
021600101018      * Controllo range
021700030117     c     D1DCD1        mult      10000         fld701            7 0
021800030120     c                   move      d1dcd2        fld701
021900030116      *
022000030120     c     d1dca1        mult      10000         fld702            7 0
022100030117     c                   move      D1DCA2        fld702
022200030116      *
022300030116     C     fld702        iflt      fld701
022400030116     C                   SETON                                        4990
022500030116     C                   GOTO      endct1
022600030116     C                   END
022700030116      * --------------
022800030825      * Decodifiche Aut
022900030825     c                   if        D1DCD2 <> 0
023000030116     C                   move      fld701        kpdr              7 0
023100030116     C     Kapd          chain     fiapd01l
023200030116     c                   if        not %Found(fiapd01l)
023300030116     C                   SETON                                        4790
023400030116     C                   GOTO      endct1
023500030116     C                   Else
023600100223     C                   MOVEL     apdrsf        d1ccd1
023700030116     C                   End
023800030120     C                   EndIf
023900030116      *
024000030825     c                   if        D1DCA2 <> 9999
024100030116     C                   move      fld702        kpdr
024200030116     C     Kapd          chain     fiapd01l
024300030116     c                   if        not %Found(fiapd01l)
024400030116     C                   SETON                                        4890
024500030116     C                   GOTO      endct1
024600030116     C                   Else
024700100223     C                   MOVEL     apdrsf        d1ccd2
024800030116     C                   End
024900030116     C                   EndIf
025000030116      * --------------
025100030116      *
025200020624      * controllo immissione delle date
025300030116     C     d1DDAL        ifeq      0
025400020308     C                   SETON                                        4390
025500030116     C                   GOTO      endct1
025600030116     C                   Else
025700030116     C                   Z-ADD     d1DDAL        G02DAT
025800000000     C                   MOVEL     *BLANK        G02ERR
025900940926     C                   CALL      'XSRDA8'
026000000000     C                   PARM                    WLBDAT
026100000000     C     G02ERR        IFEQ      '1'
026200020308     C                   SETON                                        44  90
026300030116     C                   GOTO      endct1
026400000000     C                   ENDIF
026500030116     C                   Z-ADD     G02INV        DalAmg            8 0
026600030116     C                   Z-ADD     G02dat        D1ddal
026700020307     c                   endif
026800030825      *
026900020624      * Controllo data finale
027000030116     C     d1Dal         ifeq      0
027100030127     C                   Z-ADD     d1DDal        d1Dal
027200030127     C                   End
027300030825      *
027400030116     C                   Z-ADD     d1Dal         G02DAT
027500020624     C                   MOVEL     *BLANK        G02ERR
027600020624     C                   CALL      'XSRDA8'
027700020624     C                   PARM                    WLBDAT
027800020624     C     G02ERR        IFEQ      '1'
027900020624     C                   SETON                                        42  90
028000030116     C                   GOTO      endct1
028100020624     C                   ENDIF
028200030825     c*
028300030116     C                   Z-ADD     G02INV        AlAmG             8 0
028400030116     C                   Z-ADD     G02dat        d1dal
028500030127     c*
028600030116     C     DalAmg        ifgt      AlAmg
028700020624     C                   SETON                                        4590
028800030116     C                   GOTO      endct1
028900020624     C                   END
029000030129      *
029100030116     C     endct1        ENDSR
029200030116      *--------------------------------------------------------------*
